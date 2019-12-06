# annual population survey - households by combined economic activity status
# Geography: Heart of the Southwest LEP
# Nomis 2018 (latest)
# retrieved 24 October 2019 (FB)

library(jsonlite)
library(readr)
json_url = "https://www.nomisweb.co.uk/api/v01/dataset/NM_136_1.jsonstat.json?geography=1925185548&date=latest&combined_ea_status=3...5,7...9&households_children=1&measures=20100"

empl_data <- jsonlite::fromJSON(json_url)
empl_status <- empl_data[["dimension"]][["combined_ea_status"]][["category"]][["label"]] %>% unlist()
households <- empl_data[["value"]] %>% unlist()

empl_popns <- tibble(empl_status, households)
write_csv(empl_popns, path = "hsw_lep_empl_figs.csv")
