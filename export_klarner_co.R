library(dplyr)

# expects copy of rdata in root
rdata_path <- "196slers1967to2016_20180908.RData"
out_path <- "data/klarner_co.csv"

load(rdata_path) # loads `table`

co <- table %>%
  # all CO winners — keep raw election year so Python can do
  # "most recent election before term's second year" lookup
  # that seems to be how peoeple who aren't listed in the correct term get resolved when klarner data is joined
  # im not doing the join right now, just going to make the data available
  filter(toupper(sab) == "CO", outcome == "w") %>%
  arrange(desc(year)) %>%
  distinct(candid, year, sen, .keep_all = TRUE) %>%
  mutate(
    chamber_prefix = ifelse(sen == 1, "SD", "HD"),
    district = sprintf("%s-%03d", chamber_prefix, as.integer(dno)),
    party = toupper(partyz)
  ) %>%
  select(klarner_id = candid, party, district, year, sen)

write.csv(co, out_path, row.names = FALSE)
cat("Written", nrow(co), "rows to", out_path, "\n")
