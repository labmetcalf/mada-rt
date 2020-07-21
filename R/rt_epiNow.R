# Estimating Rt for Madagascar (by region)

# Package installation (may need a few other dependencies)
# remotes::install_github("epiforecasts/EpiNow")
# remotes::install_github("epiforecasts/EpiSoon")
# install.packages("forecastHybrid)

print(Sys.getenv()["SLURM_NTASKS"])
if(is.na(Sys.getenv()["SLURM_NTASKS"])) cores <- parallel::detectCores() - 1
if(!is.na(Sys.getenv()["SLURM_NTASKS"])) cores <- as.numeric(Sys.getenv()["SLURM_NTASKS"]) - 1

cl <- parallel::makeCluster(cores) 

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

# Delay distribution
delay_defs <- readRDS("output/delay_defs.rds")[[1]]
delay_defs <- delay_defs[1:500, ]

# fit the inc delays 
incubation_defs <- EpiNow::lognorm_dist_def(mean = EpiNow::covid_incubation_period[1, ]$mean,
                                            mean_sd = EpiNow::covid_incubation_period[1, ]$mean_sd,
                                            sd = EpiNow::covid_incubation_period[1, ]$sd,
                                            sd_sd = EpiNow::covid_incubation_period[1, ]$sd_sd,
                                            max_value = 30, samples = 500)

future::plan(future::cluster, workers = cl)

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

parallel::stopCluster(cl)

future::plan("sequential")

# Then the summaries
EpiNow::regional_summary(results_dir = "output/rt_ests",
                         summary_dir = "output/rt_ests-summary",
                         target_date = "latest",
                         region_scale = "Region",
                         csv_region_label = "region",
                         log_cases = TRUE)

# Rt estimates for plotting
files <- list.files("output/rt_ests", recursive = TRUE, full.names = TRUE)
reff_files <- files[grep("latest/summarised_reff.rds", files)]
reffs <- lapply(reff_files, 
                function(x) {
                  out <- readRDS(x) 
                  out$region <- unlist(strsplit(x, "/"))[[3]]
                  out$R0_range <- NULL
                  return(out)
                })
data.table::rbindlist(reffs) -> rt_ests
write_csv(rt_ests, "latest/rt_ests.csv")

# Rt estimates for plotting
files <- list.files("output/rt_ests", recursive = TRUE, full.names = TRUE)
nowcast_files <- files[grep("latest/summarised_nowcast.rds", files)]
nowcast <- lapply(nowcast_files, 
                function(x) {
                  out <- readRDS(x) 
                  out$region <- unlist(strsplit(x, "/"))[[3]]
                  return(out)
                })
data.table::rbindlist(nowcast) -> nowcast
write_csv(nowcast, "latest/nowcast.csv")

# Summary table
rt_summary <- readRDS("output/rt_ests-summary/summary_table.rds")
rt_summary %>%
  select(-`New confirmed cases by infection date`) %>%
  mutate(date = Sys.Date()) -> rt_summary
write_csv(rt_summary, "latest/rt_summary.csv")
