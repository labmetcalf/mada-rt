# Estimating Rt for Madagascar (by region)

# Package installation (may need a few other dependencies)
# remotes::install_github("epiforecasts/EpiNow")
# remotes::install_github("epiforecasts/EpiSoon")
# install.packages("forecastHybrid)

# packages
library(EpiNow)
library(EpiSoon)
library(forecastHybrid)
library(data.table)
library(googlesheets4)
library(tidyverse)
library(lubridate)

# data (do national/Analamanga/Atsinanana)
gs4_deauth()
data   <- read_sheet("https://docs.google.com/spreadsheets/d/1oQJl4HiTviKAAhCjMmg0ipGcO79cZg6gSHrdTuQID_w/edit#gid=0", sheet = 2)

data %>%
  mutate(date = ymd(Date)) %>%
  group_by(date) %>%
  summarise(confirm = sum(Type == "N")) %>%
  complete(date = seq.Date(min(date), max(date), by = "days"), 
           fill = list(confirm = 0)) %>%
  as.data.table(.) %>%
  mutate(import_status = "local") -> reported_cases

data %>%
  filter(Location4 %in% c("Analamanga", "Atsinanana")) %>%
  mutate(date = ymd(Date), region = Location4) %>%
  group_by(date, region) %>%
  summarise(confirm = sum(Type == "N")) %>%
  ungroup() %>%
  complete(date = seq.Date(min(date), max(date), by = "days"), region,
           fill = list(confirm = 0)) %>%
  as.data.table(.) %>%
  bind_rows(mutate(reported_cases, region = "National")) %>%
  mutate(import_status = "local") -> reported_cases_region

# delay ests (pre computed)
# linelist <- 
#   data.table::fread("https://raw.githubusercontent.com/epiforecasts/NCoVUtils/master/data-raw/linelist.csv")
# delays <- linelist[!is.na(date_onset_symptoms)][, 
#                                                 .(report_delay = as.numeric(lubridate::dmy(date_confirmation) - 
#                                                                               as.Date(lubridate::dmy(date_onset_symptoms))))]
# delays <- delays$report_delay
# 
# # fit the confirmation delays (this takes time so outputting the line list & bootstrapped ests)
# delay_defs <- EpiNow::get_dist_def(delays,
#                                    bootstraps = 100, 
#                                    samples = 1000)
# delay_out <- list(delay_defs = delay_defs, 
#                    metadata = list(date_produced = Sys.Date(), 
#                                    range_dates = range(linelist$date_confirmation, na.rm = TRUE)))
# saveRDS(delay_out, "output/delay_defs.rds")
# write_csv(linelist, paste0("output/linelist_", Sys.Date(), ".csv"))

delay_defs <- readRDS("output/delay_defs.rds")[[1]]
delay_defs <- delay_defs[1:500, ]

# fit the inc delays 
incubation_defs <- EpiNow::lognorm_dist_def(mean = EpiNow::covid_incubation_period[1, ]$mean,
                                            mean_sd = EpiNow::covid_incubation_period[1, ]$mean_sd,
                                            sd = EpiNow::covid_incubation_period[1, ]$sd,
                                            sd_sd = EpiNow::covid_incubation_period[1, ]$sd_sd,
                                            max_value = 30, samples = 500)

future::plan("sequential")

# Run the pipeline (For national & regional)
EpiNow::regional_rt_pipeline(
  cases = reported_cases_region,
  delay_defs = delay_defs,
  incubation_defs = incubation_defs,
  target_folder = "output/rt_ests",
  case_limit = 0,
  min_forecast_cases = 0,
  horizon = 14,
  nowcast_lag = 10,
  approx_delay = TRUE,
  report_forecast = TRUE,
  forecast_model = function(y, ...){EpiSoon::forecastHybrid_model(
    y = y[max(1, length(y) - 21):length(y)],
    model_params = list(models = "aefz", weights = "equal"),
    forecast_params = list(PI.combination = "mean"), ...)})

# Then the summaries
EpiNow::regional_summary(results_dir = "output/rt_ests",
                         summary_dir = "output/rt_ests-summary",
                         target_date = "latest",
                         region_scale = "Region",
                         csv_region_label = "region",
                         log_cases = TRUE)
