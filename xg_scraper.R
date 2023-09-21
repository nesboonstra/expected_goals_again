# Packages

library(rvest)
library(tidyverse)




# Write function

scrape_data_xg <- function(url, league, season) {
  html <- read_html(url)
  tables <- html_elements(html,"table") %>% html_table()
  table <- tables[[1]] %>%
    mutate(seas = season) %>% 
    mutate(lig = league) %>% 
    mutate(Attendance = as.character(Attendance)) %>% 
    mutate(Rk = as.character(Rk))
  table
}

# List of domestic leagues

leagues <- c(
  9, # EPL
  11, # Serie A
  12, # La Liga
  13, # Ligue 1
  20 # Bundesliga
)

# List of seasons

seasons <- c("2017-2018","2018-2019","2019-2020","2020-2021","2021-2022","2022-2023")

# Empty tibble to build on

aggregate <- tibble()

# For loop, scraping by league and season

for (league in leagues) {
  for (season in seasons) {
    url <- paste0("https://fbref.com/en/comps/",league,"/",season)
    df <- scrape_data_xg(url,league,season)
    aggregate <- aggregate %>% bind_rows(df)
    Sys.sleep(3)
  }
}




# Once more with Europe

leagues_uefa <- c(
  8, # Champions
  19 # Europa
)

# Write new function for Europe

scrape_data_uefa_xg <- function(url, league_uefa, season) {
  html <- read_html(url)
  tables <- html_elements(html,"table") %>% html_table()
  table <- tables[[9]] %>% 
    mutate(seas = season) %>% 
    mutate(lig = league_uefa) %>% 
#    mutate(Attendance = as.character(Attendance)) %>% 
    mutate(Rk = as.character(Rk)) %>% 
    filter(is.na(MP)==0)
  table
}

# For loop again

for (league_uefa in leagues_uefa) {
  for (season in seasons) {
    url <- paste0("https://fbref.com/en/comps/",league_uefa,"/",season)
    df <- scrape_data_uefa_xg(url,league_uefa,season)
    aggregate <- aggregate %>% bind_rows(df)
    Sys.sleep(3)
  }
}



# Final dataframe cleanup

colnames(aggregate) <- c(
  "rank","squad","mp","wins",
  "draws","losses","gf","ga","gd",
  "pts","pts_per_mp","xg","xga","xgd",
  "xgd_per90","attendance","top_scorer",
  "goalkeeper","notes","seas","lig"
)

aggregate <- aggregate %>%
  mutate(seas = as_factor(seas)) %>% 
  mutate(lig = as_factor(lig)) %>% 
  mutate(dg = gf - xg) %>% 
  mutate(da = ga - xga) %>% 
  mutate(pts_per90 = pts / mp) %>% 
  mutate(xg_per90 = xg / mp) %>% 
  mutate(gf_per90 = gf / mp)

# Remove unnecessary objects

rm(df)
rm(league)
rm(leagues)
rm(league_uefa)
rm(leagues_uefa)
rm(season)
rm(seasons)
rm(url)

# Write a local copy of the dataframe

write_csv(aggregate,file="aggregate.csv")


