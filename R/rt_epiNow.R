# Estimating Rt for Madagascar (by region)

# Package installation (may need a few other dependencies)
# remotes::install_github("epiforecasts/EpiNow")
# remotes::install_github("epiforecasts/EpiSoon")
# install.packages("forecastHybrid)

print(Sys.getenv()["SLURM_NTASKS"])

cl <- parallel::makeCluster(as.numeric(Sys.getenv()["SLURM_NTASKS"]) - 1) 

# packages
library(EpiNow)
library(EpiSoon)
library(forecastHybrid)
library(data.table)
library(tidyverse)
library(lubridate)

# data (do national/Analamanga/Atsinanana)
data   <- read_csv("output/mada_cases.csv")

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

# Then the summaries
EpiNow::regional_summary(results_dir = "output/rt_ests",
                         summary_dir = "output/rt_ests-summary",
                         target_date = "latest",
                         region_scale = "Region",
                         csv_region_label = "region",
                         log_cases = TRUE)

# Rt estimates
rt_ests <- read_csv("output/rt_ests-summary/rt.csv")
write_csv(rt_ests, "latest/rt_ests.csv")

# Summary table
rt_summary <- readRDS("output/rt_ests-summary/summary_table.rds")
rt_summary %>%
  select(-`New confirmed cases by infection date`) %>%
  mutate(date = Sys.Date()) -> rt_summary
write_csv(rt_summary, "latest/rt_summary.csv")

parallel::stopCluster(cl)
