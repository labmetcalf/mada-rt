# data (do national/Analamanga/Atsinanana)
library(googlesheets4)
gs4_deauth()
data <- read_sheet("https://docs.google.com/spreadsheets/d/1oQJl4HiTviKAAhCjMmg0ipGcO79cZg6gSHrdTuQID_w/edit#gid=0",
                   sheet = 9)
readr::write_csv(data, "data/mada_cases.csv")

