import re

import pandas as pd

output_files = [
    "outputs/CO_LES_2015_2016_reestimated_PVS.csv",
    "outputs/CO_LES_2015_2016_reestimated_LN.csv",
    "outputs/CO_LES_2017_2018_reestimated_PVS.csv",
    "outputs/CO_LES_2017_2018_reestimated_LN.csv",
    "outputs/CO_LES_2019_2020_reestimated.csv",
    "outputs/CO_LES_2021_2022_reestimated.csv",
    "outputs/CO_LES_2023_2024_reestimated.csv",
]
cols = ["all_bills", "c_bills", "s_bills", "ss_bills"]

for path in output_files:
    # get term from file name
    term = re.search(r"CO_LES_(\d{4}_\d{4})_reestimated", path).group(1)
    # read old and new
    new = pd.read_csv(path)
    old = pd.read_csv(f"data/old_LES_outputs/CO_LES_{term}.csv")
    # diffs = new - old on cols
    diffs = {c: int(new[c].sum() - old[c].sum()) for c in cols}

    # comp zero les decisions
    new_zeros = sorted(new.loc[new["LES"] == 0, "sponsor"].tolist())
    old_zeros = sorted(old.loc[old["LES"] == 0, "sponsor"].tolist())

    print(path)
    print(f"    colsum diffs (new - old): {diffs}")
    print(f"    zero-LES old: {old_zeros if old_zeros else '(none)'}")
    print(f"    zero-LES new: {new_zeros if new_zeros else '(none)'}")
    print()
