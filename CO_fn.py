import numpy as np
import pandas as pd
from CO_reestimate import chamber_code_to_name

CO_STEP_TERMS = {
    "aic": [
        "committee on.+pass unamended",
        "committee on.+amended",
        "committee.+postpone indefin",
        "committee.+lay over",
        "committee on.+refer.+to",
        "witness testimony",
        "committee discussion",
        "committee.+to (?:house|senate) committee of the whole",
    ],
    "abc": [
        "committee.+to (?:house|senate) committee of the whole",
        "second reading",
        "third reading",
    ],
    "pass": [
        "^house third reading passed",
        "^senate third reading passed",
    ],
    "law": [
        "governor signed",
        "governor became law",
        "governor partial veto",
        "governor action - signed",
        "governor action - became law",
        "governor action - partial veto",
    ],
}


def evaluate_bill_hist(bill_hist: pd.DataFrame, bill_id: str, session: str):
    action = bill_hist["action"].fillna("")

    init_chamber = "Senate" if bill_id.upper().startswith("S") else "House"
    other_chamber = "House" if init_chamber == "Senate" else "Senate"

    in_idx = bill_hist.index[bill_hist["chamber"] == init_chamber]
    out_idx = bill_hist.index[bill_hist["chamber"] == other_chamber]

    if len(in_idx) > 0 and len(out_idx) > 0:
        first_in = min(in_idx)
        switch = [ix for ix in out_idx if ix > first_in]
        if switch:
            chamber_h = action[bill_hist.index < min(switch)]
        else:
            chamber_h = action
    else:
        chamber_h = action

    aic = int(chamber_h.str.contains("|".join(CO_STEP_TERMS["aic"]), regex=True, case=False).any())
    abc = int(chamber_h.str.contains("|".join(CO_STEP_TERMS["abc"]), regex=True, case=False).any())
    pc = int(chamber_h.str.contains("|".join(CO_STEP_TERMS["pass"]), regex=True, case=False).any())
    law = int(action.str.contains("|".join(CO_STEP_TERMS["law"]), regex=True, case=False).any())

    # backfill non-AIC's
    if law == 1:
        abc = 1
        pc = 1
    elif pc == 1:
        abc = 1

    # CO-specific post checks from old script
    if pc == 0 and action.str.contains("third reading passed", regex=False, case=False).any():
        abc = 1
        pc = 1

    chamber_word = "senate" if bill_id.upper().startswith("S") else "house"
    if aic == 0 and action.str.contains(chamber_word + r" committee on.+", regex=True, case=False).any():
        aic = 1

    return aic, abc, pc, law


def safe_div(num, den):
    return float(num) / float(den) if den != 0 else 0.0


def calculate_les(
    bill_data: pd.DataFrame,
    roster: pd.DataFrame,
    term: str,
    ss_weight: int,
    reg_weight: int,
    com_weight: int,
) -> pd.DataFrame:
    bill_weight = pd.Series(reg_weight, index=bill_data.index)
    bill_weight[bill_data["commem"] == 1] = com_weight
    bill_weight[bill_data["SS"] == 1] = ss_weight

    rows = []

    for chamber_code in ["H", "S"]:
        chamber_b = bill_data[bill_data["chamber_code"] == chamber_code]
        chamber_bw = bill_weight[bill_data["chamber_code"] == chamber_code]
        chamber_r = roster[roster["chamber_code"] == chamber_code]
        N = len(chamber_r)
        if N == 0:
            raise ValueError(f"Roster is empty for chamber {chamber_code} in term {term}")

        BILL_denom = (chamber_bw * chamber_b["introduced"]).sum()
        if BILL_denom == 0:
            raise ValueError(f"No matched bills for chamber {chamber_code} in term {term}")
        AIC_denom = (chamber_bw * chamber_b["action_in_comm"]).sum()
        ABC_denom = (chamber_bw * chamber_b["action_beyond_comm"]).sum()
        PASS_denom = (chamber_bw * chamber_b["passed_chamber"]).sum()
        LAW_denom = (chamber_bw * chamber_b["law"]).sum()

        for _, leg in chamber_r.iterrows():
            sponsored = chamber_b[chamber_b["data_name"] == leg["data_name"]]
            sponsored_w = chamber_bw[chamber_b["data_name"] == leg["data_name"]]

            leg_base = {
                "term": term,
                "chamber": chamber_code_to_name(chamber_code),
                "data_name": leg["data_name"],
                "sponsor": leg["sponsor"],
                "party": leg.get("party"),
                "district": leg.get("district"),
                "roster_id": leg.get("roster_id"),
                "roster_id_col": leg.get("roster_id_col"),
            }

            if sponsored.empty:
                rows.append({
                    **leg_base,
                    "LES": 0.0,
                    "BILL_wshare": 0.0,
                    "AIC_wshare": 0.0,
                    "ABC_wshare": 0.0,
                    "PASS_wshare": 0.0,
                    "LAW_wshare": 0.0,
                    "all_bills": 0,
                    "all_aic": 0,
                    "all_abc": 0,
                    "all_pass": 0,
                    "all_law": 0,
                    "ss_bills": 0,
                    "ss_aic": 0,
                    "ss_abc": 0,
                    "ss_pass": 0,
                    "ss_law": 0,
                    "s_bills": 0,
                    "s_aic": 0,
                    "s_abc": 0,
                    "s_pass": 0,
                    "s_law": 0,
                    "c_bills": 0,
                    "c_aic": 0,
                    "c_abc": 0,
                    "c_pass": 0,
                    "c_law": 0,
                    "num_sponsored_bills": 0,
                    "sponsor_pass_rate": 0.0,
                    "sponsor_law_rate": 0.0,
                    "num_cosponsored_bills": np.nan,
                })
                continue

            BILL_wshare = safe_div((sponsored_w * sponsored["introduced"]).sum(), BILL_denom)
            AIC_wshare = safe_div((sponsored_w * sponsored["action_in_comm"]).sum(), AIC_denom)
            ABC_wshare = safe_div((sponsored_w * sponsored["action_beyond_comm"]).sum(), ABC_denom)
            PASS_wshare = safe_div((sponsored_w * sponsored["passed_chamber"]).sum(), PASS_denom)
            LAW_wshare = safe_div((sponsored_w * sponsored["law"]).sum(), LAW_denom)
            LES = N / 5.0 * (BILL_wshare + AIC_wshare + ABC_wshare + PASS_wshare + LAW_wshare)

            all_bills = int(sponsored["introduced"].sum())
            all_aic = int(sponsored["action_in_comm"].sum())
            all_abc = int(sponsored["action_beyond_comm"].sum())
            all_pass = int(sponsored["passed_chamber"].sum())
            all_law = int(sponsored["law"].sum())

            ss_mask = sponsored["SS"] == 1
            s_mask = (sponsored["SS"] == 0) & (sponsored["commem"] == 0)
            c_mask = sponsored["commem"] == 1

            rows.append({
                **leg_base,
                "LES": LES,
                "BILL_wshare": BILL_wshare,
                "AIC_wshare": AIC_wshare,
                "ABC_wshare": ABC_wshare,
                "PASS_wshare": PASS_wshare,
                "LAW_wshare": LAW_wshare,
                "all_bills": all_bills,
                "all_aic": all_aic,
                "all_abc": all_abc,
                "all_pass": all_pass,
                "all_law": all_law,
                "ss_bills": int(sponsored.loc[ss_mask, "introduced"].sum()),
                "ss_aic": int(sponsored.loc[ss_mask, "action_in_comm"].sum()),
                "ss_abc": int(sponsored.loc[ss_mask, "action_beyond_comm"].sum()),
                "ss_pass": int(sponsored.loc[ss_mask, "passed_chamber"].sum()),
                "ss_law": int(sponsored.loc[ss_mask, "law"].sum()),
                "s_bills": int(sponsored.loc[s_mask, "introduced"].sum()),
                "s_aic": int(sponsored.loc[s_mask, "action_in_comm"].sum()),
                "s_abc": int(sponsored.loc[s_mask, "action_beyond_comm"].sum()),
                "s_pass": int(sponsored.loc[s_mask, "passed_chamber"].sum()),
                "s_law": int(sponsored.loc[s_mask, "law"].sum()),
                "c_bills": int(sponsored.loc[c_mask, "introduced"].sum()),
                "c_aic": int(sponsored.loc[c_mask, "action_in_comm"].sum()),
                "c_abc": int(sponsored.loc[c_mask, "action_beyond_comm"].sum()),
                "c_pass": int(sponsored.loc[c_mask, "passed_chamber"].sum()),
                "c_law": int(sponsored.loc[c_mask, "law"].sum()),
                "num_sponsored_bills": all_bills,
                "sponsor_pass_rate": safe_div(all_pass, all_bills),
                "sponsor_law_rate": safe_div(all_law, all_bills),
                "num_cosponsored_bills": np.nan,
            })

    les = pd.DataFrame(rows)
    if les.empty:
        return les

    les["LES_rank"] = (
        les.groupby("chamber")["LES"].rank(method="min", ascending=False).astype(int)
    )

    return les
