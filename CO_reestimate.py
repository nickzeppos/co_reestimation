# Re-estimaing corrupted CO terms, 15/16 - 23/24
# formatter: black
import re
import numpy as np
import pandas as pd
from pathlib import Path
from pprint import pprint
import CO_fn as fn

# consts
# paths
_HERE = Path(__file__).parent
DATA_DIR = _HERE / "data"
BILL_DIR = DATA_DIR / "bill"
COMMEM_DIR = DATA_DIR / "commem"
SS_DIR = DATA_DIR / "ss"
OLD_LES_DIR = DATA_DIR / "old_LES_outputs"
OUTPUT_DIR = _HERE / "outputs"
KEEP_TYPES = {"HB", "SB"}  # bill type filter

# output df cols
OUTPUT_COLS = [
    "LES",
    "LES_rank",
    "BILL_wshare",
    "AIC_wshare",
    "ABC_wshare",
    "PASS_wshare",
    "LAW_wshare",
    "all_bills",
    "all_aic",
    "all_abc",
    "all_pass",
    "all_law",
    "ss_bills",
    "ss_aic",
    "ss_abc",
    "ss_pass",
    "ss_law",
    "s_bills",
    "s_aic",
    "s_abc",
    "s_pass",
    "s_law",
    "c_bills",
    "c_aic",
    "c_abc",
    "c_pass",
    "c_law",
    "LES_nw",
    "num_sponsored_bills",
    "sponsor_pass_rate",
    "sponsor_law_rate",
    "num_cosponsored_bills",
]


# helpers
# arg helper for "Y-Y" -> (Y,Y)
def parse_term(term: str):
    y1, y2 = term.split("_")
    return int(y1), int(y2)


# accent handling
def standardize_accents(s: str) -> str:
    return s.replace("á", "a").replace("é", "e").replace("ó", "o").replace("í", "i")


def chamber_code_to_name(code: str) -> str:
    return "House" if code == "H" else "Senate"



def normalize_bill_details_sb_id(s: pd.Series) -> pd.Series:
    # bill details senate bill ids are 3-digit padded (SB19-001); pad to 4
    # to match bill ids in commem, and the ss bill ids we get after normalize_ss_bill_id
    return s.str.replace(r"-(\d{3})$", r"-0\1", regex=True)


def normalize_ss_bill_id(bill_no: str, year: int) -> str:
    # SS bill IDs come in two formats
    # 1 - already has YY: "HB 19-1025" or "HB19-1025" here we just zero pad and strip
    # 2 - no YY: "HB 1025" in which case we inject YY from parsed date year
    s = str(bill_no).upper().replace(" ", "")
    m = re.match(r"^(HB|SB)(\d{2}[A-Z]?)-(\d+)$", s)
    if m:
        # 1 - has yy, just zero-pad the bill number
        bill_type, yy, bill_number = m.groups()
        return f"{bill_type}{yy}-{int(bill_number):04d}"
    m = re.match(r"^(HB|SB)(\d+)$", s)
    if m:
        # 2 - no YY, inject it from the bill's year
        bill_type, bill_number = m.groups()
        yy = str(year)[-2:]
        return f"{bill_type}{yy}-{int(bill_number):04d}"
    return s



def load_roster(term: str) -> pd.DataFrame:

    # read relevant old LES df
    file_name = f"CO_LES_{term}.csv"
    path = OLD_LES_DIR / file_name
    if not path.exists():
        raise FileNotFoundError(f"Missing old les file: {path}")
    old_les_df = pd.read_csv(path)

    # take what we acutally want/need
    id_col = "legiscan_id" if "legiscan_id" in old_les_df.columns else "klarner_id"
    # grab cols
    keep = [
        c
        for c in [
            "term",
            "chamber",
            "data_name",
            "sponsor",
            "party",
            "district",
            id_col,
        ]
        if c in old_les_df.columns
    ]
    roster = old_les_df[keep].copy()
    # harmonize idcol names
    roster["roster_id"] = pd.to_numeric(roster[id_col], errors="coerce").astype("Int64")
    roster["roster_id_col"] = id_col
    roster["chamber_code"] = roster["chamber"].map({"House": "H", "Senate": "S"})
    # construct data_name for zero-LES legislators who have NA col
    # format: "f. lastname" derived from full sponsor name
    missing = roster["data_name"].isna()
    if missing.any():
        # for zero-LES legislators, derive data_name from sponsor as "f. lastname"
        roster.loc[missing, "data_name"] = roster.loc[missing, "sponsor"].apply(
            lambda s: f"{s.split()[0][0].lower()}. {' '.join(s.split()[1:]).lower()}"
        )

    return roster


def load_bill_details(term: str, roster: pd.DataFrame) -> pd.DataFrame:

    # read term years and concat
    year_1, year_2 = parse_term(term)
    acc = []
    for y in [year_1, year_2]:
        file_name = f"CO_Bill_Details_{y}.csv"
        path = BILL_DIR / file_name

        # throw on missing details file
        if not path.exists():
            raise FileNotFoundError(f"Missing details file: {path}")
        df = pd.read_csv(path)
        # 2015 file uses "sponsors" instead of "primary_sponsors"
        if y == 2015:
            df = df.rename(columns={"sponsors": "primary_sponsors"})
        acc.append(df)
    bill_details = pd.concat(acc, ignore_index=True)

    # cleaning
    # bill_number -> bill_id
    bill_details = bill_details.rename(columns={"bill_number": "bill_id"})
    # term -> arg term
    bill_details["term"] = term

    # create bill type
    bill_details["bill_type"] = (
        bill_details["bill_id"]
        .astype(str)
        .str.upper()
        .str.replace(r"[0-9].*", "", regex=True)
    )
    # filter on bill type by KEEP_TYPES
    bill_details = bill_details[bill_details["bill_type"].isin(KEEP_TYPES)]
    # drop na sponsors
    bill_details = bill_details[bill_details["primary_sponsors"].notna()]
    # There's apparently only one committee sponsored bill in all of these files
    # HB24-1026, which manifests as NA sponsor. 
    # So in practice this is just dropping 1 record from 23/24 term.

    # bill_id normalize, derive chamber
    # pad 3-digit bill numbers to 4 digits to match commem format and old R script
    # e.g. SB19-001 -> SB19-0001; HB19-1001 stays HB19-1001
    bill_details["bill_id"] = (
        bill_details["bill_id"]
        .astype(str)
        .str.upper()
        .str.replace(" ", "", regex=False)
        .pipe(normalize_bill_details_sb_id)
    )
    bill_details["chamber_code"] = np.where(
        bill_details["bill_id"].str.startswith("H"), "H", "S"
    )
    bill_details["chamber"] = bill_details["chamber_code"].map(
        {"H": "House", "S": "Senate"}
    )

    # session
    bill_details["session"] = (
        bill_details["session"].astype(str)
        + "-"
        + bill_details["session_type"].astype(str)
    )
    bill_details["session"] = bill_details["session"].str.replace(
        "-S", "-SS", regex=False
    )

    # term-specific corrections to raw sponsor strings before roster lookup
    if term == "2019_2020":
        bill_details["primary_sponsors"] = bill_details["primary_sponsors"].str.replace(
            "Robert Rodrigues", "Robert Rodriguez", regex=False
        )
    if term == "2021_2022":
        bill_details["primary_sponsors"] = bill_details["primary_sponsors"].str.replace(
            "SWoodrow", "Steven Woodrow", regex=False
        )
        bill_details["primary_sponsors"] = bill_details["primary_sponsors"].str.replace(
            "Robert Rodrigues", "Robert Rodriguez", regex=False
        )
        bill_details["primary_sponsors"] = bill_details["primary_sponsors"].str.replace(
            "Sonya Juaquez Lewis", "Sonya Jaquez Lewis", regex=False
        )
    if term == "2023_2024":
        for x, y in [
            (
                "Dafna Michaelson Jene;",
                "Dafna Michaelson Jenet;",
            ),  # first sponsor pattern
            ("Dafna Michaelson Jene ", "Dafna Michaelson Jenet "),  # cosponsor pattern
        ]:
            bill_details["primary_sponsors"] = bill_details[
                "primary_sponsors"
            ].str.replace(x, y, regex=False)

    # derive LES_sponsor from primary_sponsors via roster last-name lookup
    roster_by_chamber = {
        "H": roster[roster["chamber_code"] == "H"],
        "S": roster[roster["chamber_code"] == "S"],
    }

    bill_details["data_name"] = [
        derive_data_name(ps, roster_by_chamber[cc])
        for ps, cc in zip(
            bill_details["primary_sponsors"], bill_details["chamber_code"]
        )
    ]

    # I'm not sure we need these anymore w/ the new data.
    # term-specific overrides where automated matching resolves to the wrong legislator
    # if term == "2015_2016":
    #     bill_details.loc[bill_details["bill_id"] == "HB16-1192", "data_name"] = (
    #         "kagan d."
    #     )
    # if term == "2017_2018":
    #     fixes = {
    #         "HB17-1277": "d. mitsch bush",
    #         "HB17-1166": "c. navarro",
    #         "HB17-1104": "c. navarro",
    #         "HB17-1044": "d. mitsch bush",
    #         "HB17-1150": "c. navarro",
    #         "HB18-1133": "s. lebsock",
    #         "SB18-0043": "k. grantham",
    #     }
    #     for bill_id, name in fixes.items():
    #         bill_details.loc[bill_details["bill_id"] == bill_id, "data_name"] = name
    return bill_details


def infer_action_chamber(bill_histories: pd.DataFrame) -> pd.DataFrame:
    # 2015 bill history has no chamber col, and we need it evaluate bil history
    # split on session
    is_2015_rs = bill_histories["session"] == "2015-RS"
    hist_2015 = bill_histories[is_2015_rs].copy()
    actions = hist_2015["action"].str.lower()

    # init empty chamber series same size as 2015 slice of history df, indexed on 2015 hist
    chamber = pd.Series([None] * len(hist_2015), index=hist_2015.index, dtype="object")

    # set chamber value by checking if action contains some phrases
    chamber.loc[
        actions.str.contains(
            r"introduced in house|sent to senate|sent back to senate|^house|speaker of the house",
            regex=True,
        )
    ] = "House"
    chamber.loc[
        actions.str.contains(
            r"introduced in senate|sent to house|sent back to house|^senate|president of the senate",
            regex=True,
        )
    ] = "Senate"
    chamber.loc[
        actions.str.contains(
            r"bill is signed into law|bill is vetoed|^governor|sent to the governor",
            regex=True,
        )
    ] = "Governor"

    # use as chamber col
    hist_2015["chamber"] = chamber

    # this will leave us with a fair amount of empty chamber values, which we solve by forward fill
    # e.g., House, NA, Senate, NA -> House, House, Senate, Senate
    # This is how the old script does it
    # lines 394–400 in CO - Estimate LES AV.R
    hist_2015["chamber"] = hist_2015.groupby(["bill_id", "session"], sort=False)[
        "chamber"
    ].ffill()

    not_2015_rs = bill_histories[bill_histories["session"] != "2015-RS"]
    bill_histories = pd.concat([not_2015_rs, hist_2015], ignore_index=True)
    bill_histories = bill_histories.sort_values(
        ["session", "bill_id", "order"], kind="stable"
    )
    return bill_histories


def load_bill_histories(term: str) -> pd.DataFrame:

    # read term years and concat
    year_1, year_2 = parse_term(term)
    acc = []

    for y in [year_1, year_2]:
        file_name = f"CO_Bill_Histories_{y}.csv"
        path = BILL_DIR / file_name

        # throw on missing hist file
        if not path.exists():
            raise FileNotFoundError(f"Missing history file: {path}")
        acc.append(pd.read_csv(path))

    bill_histories = pd.concat(acc, ignore_index=True)

    bill_histories = bill_histories.rename(columns={"bill_number": "bill_id"})
    bill_histories["term"] = term
    bill_histories["session"] = (
        bill_histories["session"].astype(str)
        + "-"
        + bill_histories["session_type"].astype(str)
    )
    bill_histories["session"] = bill_histories["session"].str.replace(
        "-S", "-SS", regex=False
    )
    bill_histories["bill_id"] = (
        bill_histories["bill_id"]
        .astype(str)
        .str.upper()
        .str.replace(" ", "", regex=False)
        .pipe(normalize_bill_details_sb_id)
    )
    bill_histories = bill_histories.sort_values(
        ["session", "bill_id", "order"], kind="stable"
    )

    if year_1 == 2015:
        bill_histories = infer_action_chamber(bill_histories)

    return bill_histories


def derive_data_name(primary_sponsors: str, roster: pd.DataFrame) -> str:
    # primary_sponsors is a row value from bill details
    # first sponsor is for our purposes the sponsor who will receive credit
    raw = primary_sponsors.split(";")[0].strip()

    # strip title prefix, 2015 doesn't have role prefix but regex should just ~noop on that
    raw = re.sub(r"^Representative |^Senator ", "", raw)
    # normalize
    raw = standardize_accents(raw.lower()).strip()

    # 2015 disambiguation format: "lastname initial." (e.g. "becker j.") — strip the
    # trailing initial, keep it around to narrow ambiguous lastname matches below.
    trailing_m = re.match(r"^(.+) ([a-z])\.$", raw)
    trailing_initial = trailing_m.group(2) if trailing_m else None
    if trailing_m:
        raw = trailing_m.group(1)

    # extract last name portion from sponsor string
    # 2015: bare last name e.g. "pettersen"
    # 2016+: "firstname [middle] lastname" e.g. "diane mitsch bush" -> "mitsch bush"
    # leading initial e.g. "j. paul brown" -> skip "j.", treat "paul brown" as "firstname last"
    parts = raw.split()
    if len(parts) >= 2 and re.match(r"^[a-z]\.$", parts[0]):
        parts = parts[1:]
    sponsor_last = parts[0] if len(parts) == 1 else " ".join(parts[1:])

    roster_names = roster["data_name"].tolist()

    # look up in roster by last name — strip initial from data_name to compare
    # handle both "initial. lastname" and "lastname initial." formats
    matches = []
    for data_name in roster_names:
        dn_last = re.sub(r"^[a-z]\. ", "", data_name)
        dn_last = re.sub(r" [a-z]\.$", "", dn_last)
        if dn_last == sponsor_last:
            matches.append(data_name)

    # fallback: compound last names where bill data uses only the final surname 
    # e.g. "beth humenik" -> "humenik" matches "martinez humenik b." via suffix
    if not matches:
        for data_name in roster_names:
            dn_last = re.sub(r"^[a-z]\. ", "", data_name)
            dn_last = re.sub(r" [a-z]\.$", "", dn_last)
            if dn_last.endswith(" " + sponsor_last):
                matches.append(data_name)

    if len(matches) == 1:
        return matches[0]
    if len(matches) == 0:
        raise ValueError(f"No roster match found for sponsor '{raw}'")

    # multiple last-name matches — try to narrow by first initial of sponsor.
    # prefer the 2015 trailing initial (e.g. "becker j." disambiguates to "becker j.")
    # and fall back to first char of the sponsor's first name token.
    first_initial = trailing_initial
    if first_initial is None and len(parts) >= 2:
        first_initial = parts[0][0]
    if first_initial is not None:
        # data_name "initial. lastname" -> initial is first char
        # data_name "lastname initial." -> initial is last word's first char
        narrowed = [
            dn
            for dn in matches
            if re.search(r"(?:^|(?<= )){}\.".format(re.escape(first_initial)), dn)
        ]
        if len(narrowed) == 1:
            return narrowed[0]

    # raise on ambiguity so we can root it out
    raise ValueError(f"Ambiguous roster match for sponsor '{raw}': {matches}")


def compute_leg_achievement(bills: pd.DataFrame, hist: pd.DataFrame) -> pd.DataFrame:
    # map bill_id, session -> subset in bill history
    # ie pre loop filter
    hist_keys = {
        (bid, ses): g
        for (bid, ses), g in hist.groupby(["bill_id", "session"], sort=False)
    }
    acc = []
    
    for _, b in bills.iterrows():
        key = (b["bill_id"], b["session"])
        # grab the bill history
        h = hist_keys.get(key, pd.DataFrame(columns=hist.columns))
        # evaluate
        aic, abc, pc, law = fn.evaluate_bill_hist(h, b["bill_id"], b["session"])
        # accumulate
        acc.append(
            {
                "bill_id": b["bill_id"],
                "term": b["term"],
                "session": b["session"],
                "data_name": b["data_name"],
                "title": b.get("title", ""),
                "introduced": 1,
                "action_in_comm": aic,
                "action_beyond_comm": abc,
                "passed_chamber": pc,
                "law": law,
                "chamber_code": b["chamber_code"],
            }
        )
    return pd.DataFrame(acc)


def load_term_commem(term: str) -> pd.DataFrame:
    # 2019+ terms have their own per-term file; older terms are in a single combined file
    term_path = COMMEM_DIR / f"CO_Commem_Bills_{term}.csv"
    combined_path = COMMEM_DIR / "CO_Commem_Bills.csv"

    if term_path.exists():
        # prefer the term specific commem sheets
        c = pd.read_csv(term_path)
    elif combined_path.exists():
        # use combined sheet if no term specific sheet
        c = pd.read_csv(combined_path)
        c = c[c["term"] == term].copy()
        if c.empty:
            raise ValueError(f"No commem rows found for term {term} in {combined_path}")
        c["session"] = c["session"].astype(str).str.replace("-S", "-SS", regex=False)

    c["bill_id"] = c["bill_id"].astype(str).str.upper().str.replace(" ", "", regex=False)
    c["commem"] = c["commem"].astype(int)
    return c[["bill_id", "term", "session", "commem"]]


def load_term_ss(term: str) -> pd.DataFrame:
    # SS files are annual, so concat both years of the term
    year_1, year_2 = parse_term(term)
    parts = []
    for y in [year_1, year_2]:
        p = SS_DIR / f"CO_SS_Bills_{y}.csv"
        if not p.exists():
            raise FileNotFoundError(f"Missing SS bills file: {p}")
        parts.append(pd.read_csv(p))
    ss = pd.concat(parts, ignore_index=True)
    # parse the PVS date string to extract year; "Sept" and trailing dots are
    # common formatting quirks in the source data
    dt = pd.to_datetime(
        ss["Date"].astype(str)
        .str.replace("Sept", "Sep", regex=False)
        .str.replace(".", "", regex=False),
        format="mixed",
    )
    # ss_year drives bill ID normalization (the YY in e.g. HB19-1025)
    ss["ss_year"] = dt.dt.year.astype(int)
    ss["bill_id"] = [
        normalize_ss_bill_id(b, y) for b, y in zip(ss["Bill No"], ss["ss_year"])
    ]
    ss["SS"] = 1
    return ss[["bill_id", "ss_year", "SS"]].drop_duplicates()


def load_term_ss_master(term: str) -> pd.DataFrame:
    # The LN master SS_Bills.csv covers all states/years in one file.
    # bill_num is already in "hbYY-NNNN" / "sbYY-NNNN" form; just uppercase.
    path = SS_DIR / "SS_Bills.csv"
    if not path.exists():
        raise FileNotFoundError(f"Missing master SS file: {path}")
    df = pd.read_csv(path)
    year_1, year_2 = parse_term(term)
    df = df[(df["state"] == "CO") & (df["year"].isin([year_1, year_2]))].copy()
    df["bill_id"] = (
        df["bill_num"].astype(str).str.upper().str.replace(" ", "", regex=False)
    )
    df["ss_year"] = df["year"].astype(int)
    df["SS"] = 1
    return df[["bill_id", "ss_year", "SS"]].drop_duplicates()


# Apply manual SS bill id fixes
SS_ID_FIXES = {
    "2019_2020": {"SB19-1025": "HB19-1025"},
    "2021_2022": {"HB21-0002": "HB21-1002", "HB21-0003": "HB21-1003"},
    "2023_2024": {
        "SB23-1196": "HB23-1196",
        "SB23-1006": "HB23-1006",
        "SB24-1091": "HB24-1091",
    },
}


def fix_ss_bill_ids(ss: pd.DataFrame, term: str) -> pd.DataFrame:
    fixes = SS_ID_FIXES.get(term, {})
    ss = ss.copy()
    ss["bill_id"] = ss["bill_id"].replace(fixes)
    return ss


def apply_ss_and_commem(
    stages: pd.DataFrame, ss_term: pd.DataFrame, commem_term: pd.DataFrame
) -> pd.DataFrame:
    out = stages
    # derive year from session string (eg "2019-RS" -> 2019) for SS matching
    out["ss_year"] = out["session"].astype(str).str[:4].astype(int)
    out["SS"] = 0

    # flag bills that appear in the SS list, matched on bill_id + year
    if not ss_term.empty:
        for _, s in ss_term.iterrows():
            cand_idx = out.index[
                (out["bill_id"] == s["bill_id"]) & (out["ss_year"] == s["ss_year"])
            ]
            out.loc[cand_idx, "SS"] = 1

    # join commem flags; if a bill is both SS and commem, SS wins
    out = out.merge(commem_term, on=["bill_id", "term", "session"], how="left")
    out["commem"] = out["commem"].fillna(0).astype(int)
    out.loc[(out["SS"] == 1) & (out["commem"] == 1), "commem"] = 0
    return out.drop(columns=["ss_year"])



def main():
    # 15/16 and 17/18 get dual outputs: one from the PVS annual files
    # and one from the LN master file, since the two disagree materially
    # on SS coverage for those terms.
    DUAL_TERMS = {"2015_2016", "2017_2018"}

    for term in ["2015_2016", "2017_2018", "2019_2020", "2021_2022", "2023_2024"]:
        print(f"Re-estimating CO {term}")

        roster = load_roster(term)
        details = load_bill_details(term, roster)
        histories = load_bill_histories(term)

        leg_achievement = compute_leg_achievement(details, histories)
        commem = load_term_commem(term)

        if term in DUAL_TERMS:
            sources = [
                ("_PVS", fix_ss_bill_ids(load_term_ss(term), term)),
                ("_LN", fix_ss_bill_ids(load_term_ss_master(term), term)),
            ]
        else:
            sources = [("", fix_ss_bill_ids(load_term_ss(term), term))]

        # term-specific legislator exclusions applied before LES calc
        if term == "2023_2024":
            roster = roster[roster["sponsor"] != "Robert Rankin"].reset_index(drop=True)

        for suffix, ss in sources:
            bill_data = apply_ss_and_commem(leg_achievement.copy(), ss, commem)

            les = fn.calculate_les(
                bill_data, roster, term, ss_weight=10, reg_weight=5, com_weight=1
            )
            les_nw = fn.calculate_les(
                bill_data, roster, term, ss_weight=5, reg_weight=5, com_weight=5
            )

            key = ["term", "chamber", "data_name", "sponsor"]
            les = les.merge(
                les_nw[key + ["LES"]].rename(columns={"LES": "LES_nw"}),
                on=key,
                how="left",
            )

            OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
            out_path = OUTPUT_DIR / f"CO_LES_{term}_reestimated{suffix}.csv"
            les.to_csv(out_path, index=False)
            print(f"Written to {out_path}")

            bills_path = (
                OUTPUT_DIR / f"CO_LES_{term}_reestimated_coded_bills{suffix}.csv"
            )
            bill_data.to_csv(bills_path, index=False)
            print(f"Written to {bills_path}")


if __name__ == "__main__":
    main()
