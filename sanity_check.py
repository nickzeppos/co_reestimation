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
prefixes = ["all", "ss", "s", "c"]
steps = ["bills", "aic", "abc", "pass", "law"]
cols = [f"{p}_{s}" for p in prefixes for s in steps]

for path in output_files:
    # get term from file name
    term = re.search(r"CO_LES_(\d{4}_\d{4})_reestimated", path).group(1)
    # read old and new
    new = pd.read_csv(path)
    old = pd.read_csv(f"data/old_LES_outputs/CO_LES_{term}.csv")
    # diffs = new - old on cols, grouped by prefix for readability
    diffs_by_prefix = {
        p: {s: int(new[f"{p}_{s}"].sum() - old[f"{p}_{s}"].sum()) for s in steps}
        for p in prefixes
    }

    # comp zero les decisions
    new_zeros = sorted(new.loc[new["LES"] == 0, "sponsor"].tolist())
    old_zeros = sorted(old.loc[old["LES"] == 0, "sponsor"].tolist())

    print(path)
    print("    colsum diffs (new - old):")
    for p in prefixes:
        print(f"        {p}: {diffs_by_prefix[p]}")
    print(f"    zero-LES old: {old_zeros if old_zeros else '(none)'}")
    print(f"    zero-LES new: {new_zeros if new_zeros else '(none)'}")
    print()
