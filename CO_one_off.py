# One off estimation script for CO.
# Re-estimate LES using new bill details/history while treating old LES
# roster rows as truth for legislator identity metadata.

import re
import numpy as np
import pandas as pd
import CO_stuff.CO_utils as U
import CO_stuff.CO_fn as Fn



# Given a term (yyyy-yyyy), load and clean corresponding bill details files
def load_term_details(term: str, roster: pd.DataFrame) -> pd.DataFrame:
    # split term arg
    y1, y2 = U.parse_term(term)
    parts = []
    
    # grab details files for years of the term
    for y in [y1, y2]:
        p = U.BILL_DIR / f"CO_Bill_Details_{y}.csv"
        if not p.exists():
            raise FileNotFoundError(f"Missing details file: {p}")
        parts.append(pd.read_csv(p))
    bills = pd.concat(parts, ignore_index=True)
    bills = bills.rename(columns={"bill_number": "bill_id"})
    bills["bill_type"] = bills["bill_id"].astype(str).str.upper().str.replace(r"[0-9].*", "", regex=True)
    
    # filter to bill types of interest (keep types)
    bills = bills[bills["bill_type"].isin(U.KEEP_TYPES)].copy()

    # 2015 details file uses "sponsors" instead of "primary_sponsors". 
    if term == "2015_2016" and "primary_sponsors" not in bills.columns:
        bills = bills.rename(columns={"sponsors": "primary_sponsors"})

    # term col
    bills["term"] = term

    # bill_id normalization and chamber derivation (needed before sponsor lookup)
    bills["bill_id"] = bills["bill_id"].astype(str).str.upper().str.replace(" ", "", regex=False)
    bills["chamber_code"] = np.where(bills["bill_id"].str.startswith("H"), "H", "S")
    bills["chamber"] = bills["chamber_code"].map({"H": "House", "S": "Senate"})

    # session col
    bills["session"] = bills["session"].astype(str) + "-" + bills["session_type"].astype(str)
    bills["session"] = bills["session"].str.replace("-S", "-SS", regex=False)

    # Deriving sponsor column
    # initially equal to primary_sponsors
    bills["sponsor"] = bills["primary_sponsors"]
    # Drop everyhing after initial ;
    bills["sponsor"] = bills["sponsor"].str.replace(r";.+", "", regex=True)
    bills["sponsor"] = bills["sponsor"].str.replace(r"^Rep\. |^Sen\. ", "", regex=True)
    bills["sponsor"] = bills["sponsor"].str.lower().str.replace(r";.+|/.+", "", regex=True)
    bills["sponsor"] = U.standardize_accents(bills["sponsor"])  # match old script behavior

    # CO term-specific sponsor fixes in old script for 2015+
    if term == "2015_2016":
        bills.loc[bills["bill_id"] == "HB16-1192", "sponsor"] = "d. kagan"
    if term == "2017_2018":
        fixes = {
            "HB17-1277": "d. mitsch bush",
            "HB17-1166": "c. navarro",
            "HB17-1104": "c. navarro",
            "HB17-1044": "d. mitsch bush",
            "HB17-1150": "c. navarro",
            "HB18-1133": "s. lebsock",
            "SB18-0043": "k. grantham",
        }
        for bid, name in fixes.items():
            bills.loc[bills["bill_id"] == bid, "sponsor"] = name

    bills["LES_sponsor"] = bills["sponsor"].str.strip()

    # Normalize sponsor names to match data_name format in old LES files.
    # 2015_2016 data_name is "lastname initial."; 2017+ is "initial. lastname".
    # Bill sponsor data from 2016 onward is full names; 2015 data is already "initial. lastname".
    if term == "2015_2016":
        # Both sessions need "lastname initial." — 2015-RS is bare last name, 2016-RS is "First Last"
        def to_last_initial(name):
            m = re.match(r"^([a-z])\. (.+)$", name)
            if m:
                return f"{m.group(2)} {m.group(1)}."
            parts = name.split()
            return f"{' '.join(parts[1:])} {parts[0][0]}." if len(parts) >= 2 else name
        bills["LES_sponsor"] = bills["LES_sponsor"].map(to_last_initial)

        # 2015-RS sponsors are bare last names — look up data_name from roster by last name + chamber
        last_to_data_name = {}
        for _, row in roster.iterrows():
            last = row["data_name"].split()[0]
            key = (row["chamber_code"], last)
            last_to_data_name[key] = None if key in last_to_data_name else row["data_name"]
        is_2015 = bills["session"] == "2015-RS"
        bills.loc[is_2015, "LES_sponsor"] = bills[is_2015].apply(
            lambda r: last_to_data_name.get((r["chamber_code"], r["LES_sponsor"])) or r["LES_sponsor"],
            axis=1,
        )
    else:
        # 2017+ data_name is "initial. lastname"; bill data is full names
        def to_initial_last(name):
            if re.match(r"^[a-z]\. ", name):  # already correct (manual fixes)
                return name
            parts = name.split()
            return f"{parts[0][0]}. {' '.join(parts[1:])}" if len(parts) >= 2 else name
        bills["LES_sponsor"] = bills["LES_sponsor"].map(to_initial_last)

    bills = bills[bills["LES_sponsor"].notna() & (bills["LES_sponsor"] != "")].copy()

    keep_cols = [c for c in ["bill_id", "term", "session", "title", "LES_sponsor", "bill_url", "chamber_code", "chamber"] if c in bills.columns]
    return bills[keep_cols]


def load_term_history(term: str) -> pd.DataFrame:
    y1, y2 = U.parse_term(term)
    parts = []
    for y in [y1, y2]:
        p = U.BILL_DIR / f"CO_Bill_Histories_{y}.csv"
        if not p.exists():
            raise FileNotFoundError(f"Missing history file: {p}")
        parts.append(pd.read_csv(p))
    hist = pd.concat(parts, ignore_index=True)

    hist = hist.rename(columns={"bill_number": "bill_id"})
    hist["term"] = term
    hist["session"] = hist["session"].astype(str) + "-" + hist["session_type"].astype(str)
    hist["session"] = hist["session"].str.replace("-S", "-SS", regex=False)
    hist["bill_id"] = hist["bill_id"].astype(str).str.upper().str.replace(" ", "", regex=False)
    hist = hist.sort_values(["session", "bill_id", "order"], kind="stable")

    if "chamber" not in hist.columns:
        hist["chamber"] = np.nan
    if y1 == 2015:
        m = hist["session"] == "2015-RS"
        hist_2015 = U.infer_legacy_chamber(hist[m])
        hist = pd.concat([hist[~m], hist_2015], ignore_index=True)
        hist = hist.sort_values(["session", "bill_id", "order"], kind="stable")

    return hist


def build_bill_stages(bills: pd.DataFrame, hist: pd.DataFrame) -> pd.DataFrame:
    hist_keys = {(bid, ses): g for (bid, ses), g in hist.groupby(["bill_id", "session"], sort=False)}

    rows = []
    for _, b in bills.iterrows():
        key = (b["bill_id"], b["session"])
        h = hist_keys.get(key, pd.DataFrame(columns=hist.columns))
        aic, abc, pc, law = Fn.evaluate_bill_hist(h, b["bill_id"], b["session"])
        rows.append(
            {
                "bill_id": b["bill_id"],
                "term": b["term"],
                "session": b["session"],
                "LES_sponsor": b["LES_sponsor"],
                "title": b.get("title", ""),
                "bill_url": b.get("bill_url", np.nan),
                "introduced": 1,
                "action_in_comm": aic,
                "action_beyond_comm": abc,
                "passed_chamber": pc,
                "law": law,
                "chamber_code": b["chamber_code"],
            }
        )

    return pd.DataFrame(rows)


def load_term_commem(term: str) -> pd.DataFrame:
    path = U.COMMEM_DIR / "CO_Commem_Bills.csv"
    if not path.exists():
        return pd.DataFrame(columns=["bill_id", "term", "session", "commem"])
    c = pd.read_csv(path)
    c = c[c["term"] == term].copy()
    if c.empty:
        return c
    c["bill_id"] = c["bill_id"].astype(str).str.upper().str.replace(" ", "", regex=False)
    c["session"] = c["session"].astype(str).str.replace("-S", "-SS", regex=False)
    c["commem"] = c["commem"].fillna(0).astype(int)
    return c[["bill_id", "term", "session", "commem"]].drop_duplicates()


def load_term_ss(term: str) -> pd.DataFrame:
    y1, y2 = U.parse_term(term)
    parts = []
    for y in [y1, y2]:
        p = U.SS_DIR / f"CO_SS_Bills_{y}.csv"
        if p.exists():
            d = pd.read_csv(p)
            d["_fallback_year"] = y
            parts.append(d)
    if not parts:
        return pd.DataFrame(columns=["bill_id", "ss_year", "title_norm", "SS"])

    ss = pd.concat(parts, ignore_index=True)
    dt = pd.to_datetime(
        ss["Date"].astype(str).str.replace("Sept", "Sep", regex=False).str.replace(".", "", regex=False),
        errors="coerce",
    )
    ss["ss_year"] = dt.dt.year.fillna(ss["_fallback_year"]).astype(int)
    ss["bill_id"] = [U.normalize_ss_bill_id(b, y) for b, y in zip(ss["Bill No"], ss["ss_year"])]
    ss["title_norm"] = ss["Title"].map(U.normalize_bill_title)
    ss["SS"] = 1
    return ss[["bill_id", "ss_year", "title_norm", "SS"]].drop_duplicates()


def apply_ss_and_commem(stages: pd.DataFrame, ss_term: pd.DataFrame, commem_term: pd.DataFrame) -> pd.DataFrame:
    out = stages.copy()
    out["ss_year"] = out["session"].astype(str).str[:4].astype(int)
    out["title_norm"] = out["title"].map(U.normalize_bill_title)
    out["SS"] = 0

    if not ss_term.empty:
        for _, s in ss_term.iterrows():
            cand_idx = out.index[(out["bill_id"] == s["bill_id"]) & (out["ss_year"] == s["ss_year"])]
            if len(cand_idx) == 0:
                continue
            if len(cand_idx) == 1:
                out.loc[cand_idx, "SS"] = 1
                continue
            title_idx = out.index[(out.index.isin(cand_idx)) & (out["title_norm"] == s["title_norm"])]
            if len(title_idx) > 0:
                out.loc[title_idx, "SS"] = 1
            else:
                out.loc[cand_idx, "SS"] = 1

    out = out.merge(commem_term, on=["bill_id", "term", "session"], how="left")
    out["commem"] = out["commem"].fillna(0).astype(int)
    out.loc[(out["SS"] == 1) & (out["commem"] == 1), "commem"] = 0

    return out.drop(columns=["ss_year", "title_norm"])


def build_term_output(term: str, old_df: pd.DataFrame) -> pd.DataFrame:
    print(f"\n--- Re-estimating {term} ---")
    roster = U.build_roster(old_df)
    bills = load_term_details(term, roster)
    hist = load_term_history(term)

    # Drop bills where that session has no history rows
    valid_sessions = set(hist["session"].unique())
    bills = bills[bills["session"].isin(valid_sessions)].copy()

    stages = build_bill_stages(bills, hist)
    commem_term = load_term_commem(term)
    ss_term = load_term_ss(term)
    bills_scored = apply_ss_and_commem(stages, ss_term, commem_term)

    les_main = Fn.calculate_les(bills_scored, roster, term, ss_weight=10, reg_weight=5, com_weight=1)
    les_nw = Fn.calculate_les(bills_scored, roster, term, ss_weight=5, reg_weight=5, com_weight=5)

    key = ["term", "chamber", "data_name", "sponsor"]
    if les_main.empty:
        merged = roster[["term", "chamber", "data_name", "sponsor"]].copy()
    else:
        merged = les_main.copy()

    if not les_nw.empty:
        merged = merged.merge(
            les_nw[key + ["LES"]].rename(columns={"LES": "LES_nw"}),
            on=key,
            how="left",
        )
    else:
        merged["LES_nw"] = np.nan

    roster_keys = [c for c in ["term", "chamber", "data_name", "sponsor", "party", "district", "roster_id", "roster_id_col"] if c in roster.columns]
    merged = roster[roster_keys].merge(merged, on=["term", "chamber", "data_name", "sponsor"], how="left")

    for c in U.OUTPUT_COLS:
        if c not in merged.columns:
            merged[c] = np.nan

    
    # Match old schema exactly per term
    out = merged.copy()
    id_col = "legiscan_id" if "legiscan_id" in old_df.columns else "klarner_id"
    out[id_col] = out["roster_id"]

    # keep optional columns if present in old files
    if "klarner_name" in old_df.columns and "klarner_name" not in out.columns:
        old_lookup = old_df[["term", "chamber", "data_name", "klarner_name"]].drop_duplicates()
        out = out.merge(old_lookup, on=["term", "chamber", "data_name"], how="left")

    # Ensure all old columns exist
    for c in old_df.columns:
        if c not in out.columns:
            out[c] = np.nan

    out = out[old_df.columns].copy()
    out = out.sort_values(["term", "chamber", "LES_rank", "sponsor"], kind="stable")
    return out


def main():
    U.OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    old_frames = U.load_old_les_frames()
    terms = sorted(old_frames.keys())

    for term in terms:
        out = build_term_output(term, old_frames[term])
        out_path = U.OUTPUT_DIR / f"CO_LES_{term}_reestimated.csv"
        out.to_csv(out_path, index=False)
        print(f"wrote {out_path}")


if __name__ == "__main__":
    main()
