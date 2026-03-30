library(dplyr)

# expects copy of rdata in root
rdata_path <- "196slers1967to2016_20180908.RData"
out_path <- "data/klarner_co.csv"

load(rdata_path) # loads `table`

co <- table %>%
  # filter on CO winners
  filter(toupper(sab) == "CO", outcome == "w") %>%
  mutate(term = paste0(year + 1, "_", year + 2)) %>%
  # take only one outcome per term, arrange by year distinct
  # effectively gives us most recent/last outcome in a given term
  arrange(desc(year)) %>%
  distinct(candid, term, sen, .keep_all = TRUE) %>%
  # fix up the columns so that we don't have to do any more cleaning in python
  mutate(
    chamber_prefix = ifelse(sen == 1, "SD", "HD"),
    district = sprintf("%s-%03d", chamber_prefix, as.integer(dno)), # district col should be prefixed
    party = toupper(partyz) # this seems to be the party col the old script uses 
  ) %>%
  # only take stuff we need to reestimate
  select(klarner_id = candid, party, district, term, sen)

write.csv(co, out_path, row.names = FALSE)
