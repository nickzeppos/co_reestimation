## Data

- `data/bill/` — newly scraped bill details and histories 
- `data/ss/` — SS files
- `data/commem/` — commem bill data (combined file for pre-19, per-term files for 2019+)
- `data/klarner_co.csv` - export_klarner script output
- `data/old_LES_outputs/` — original (i.e. based on old data scrape) LES outputs used to build the legislator roster

## Files

- `CO_reestimate.py` — main pipeline (data loading, bill stage computation, LES calculation)
- `CO_fn.py` — bill history evaluation and LES scoring functions
- `CO - Estimate LES.R` — original R estimation script (reference for klarner data shape for me, mostly)
- `export_klarner_co.R` / `196slers1967to2016_20180908.RData` — klarner export script + orig rdata file

## TODO
- I have made the klarner data avaialble (from the .rdata file on dropbox) to the best of my ability, but I didn't actually merge anything in from those data sets. I assume this is where party/district info comes from for pre-18 info, but I didn't want to overstep (just wanted to match the old LES outputs)
- Given that you'll note that party and district columns do exist in the 15/16 and 17/18 reestimations, they're just empty.
