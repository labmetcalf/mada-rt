# Sample delay distribution (this takes a while so only doing this once)

# packages
library(EpiNow)
library(EpiSoon)
library(forecastHybrid)
library(data.table)
library(googlesheets4)
library(tidyverse)
library(lubridate)

# delay ests (pre computed)
linelist <-
  data.table::fread("https://raw.githubusercontent.com/epiforecasts/NCoVUtils/master/data-raw/linelist.csv")
delays <- linelist[!is.na(date_onset_symptoms)][,
                                                .(report_delay = as.numeric(lubridate::dmy(date_confirmation) -
                                                                              as.Date(lubridate::dmy(date_onset_symptoms))))]
delays <- delays$report_delay

# fit the confirmation delays (this takes time so outputting the line list & bootstrapped ests)
delay_defs <- EpiNow::get_dist_def(delays,
                                   bootstraps = 100,
                                   samples = 1000)
delay_out <- list(delay_defs = delay_defs,
                   metadata = list(date_produced = Sys.Date(),
                                   range_dates = range(linelist$date_confirmation, na.rm = TRUE)))
saveRDS(delay_out, "output/delay_defs.rds")
write_csv(linelist, paste0("output/linelist_", Sys.Date(), ".csv"))
