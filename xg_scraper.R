library(rvest)
library(tidyverse)


scrape_data_xg <- function(url, season) {
  html <- read_html(url)
  tables <- html_elements(html,"table") %>% html_table()
  table <- tables[[1]] %>% mutate(seas = season) %>% select(!Attendance)
  table
}

seasons <- c("2017-2018","2018-2019","2019-2020","2020-2021","2021-2022","2022-2023")

aggregate <- tibble()

for (season in seasons) {
  url <- paste0("https://fbref.com/en/comps/9/",season,"/",season,"-Premier-League-Stats")
  df <- scrape_data_xg(url,season)
  aggregate <- aggregate %>% bind_rows(df)
  Sys.sleep(3)
}

colnames(aggregate) <- c(
  "rank","squad","mp","wins",
  "draws","losses","gf","ga","gd",
  "pts","pts_per_mp","xg","xga","xgd",
  "xgd_per_90","top_scorer","goalkeeper",
  "notes","seas"
)

aggregate <- aggregate %>%
  mutate(seas = as_factor(seas)) %>% 
  mutate(dg = gf - xg) %>% 
  mutate(da = ga - xga)

rm(df)
rm(season)
rm(seasons)
rm(url)

write_csv(aggregate,file="aggregate.csv")


