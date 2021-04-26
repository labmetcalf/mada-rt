# Estimating Rt for Madagascar (by region)

# sub_cmd:=-t 1 -n 9 -jn rt -wt 1m -sn -@

arg <- commandArgs(trailingOnly = TRUE)

# Set up on cluster ------
source("R/utils.R")
set_up <- setup_cl(mpi = FALSE)

cl <- make_cl(set_up$ncores)
print(paste("Cluster size:", cl_size(cl)))

if(!set_up$slurm) fp <- here::here else fp <- cl_safe

# packages
library(EpiNow)
library(EpiSoon)
library(forecastHybrid)
library(data.table)
library(googlesheets4)
library(tidyverse)
library(lubridate)
library(magrittr)
library(future)

# data (do national/Analamanga/Atsinanana)
data <- read_csv("data/mada_cases.csv")

# Filter data to a max 4 month window (excluding the past 3 days)
# And starting from the date they began daily reporting again
date_min <- case_when(Sys.Date() - 4 * 30 > ymd("2021-03-01") ~ ymd(Sys.Date() - 4 * 30),
                      TRUE ~ ymd("2021-03-01"))
data %>%
  mutate(date = ymd(week), import_status = "local") %>%
  filter(date < Sys.Date() & date > date_min) %>% # just including today since these are date reports
  group_by(date, region, import_status) %>%
  summarise(confirm = sum(cases)) %>%
  tidyr::complete(date = seq.Date(min(date), max(date), by = "days"), 
                  fill = list(confirm = 0)) -> reported_cases_regional

reported_cases_regional %<>%
  group_by(date, import_status) %>%
  summarize(confirm = sum(confirm, na.rm = TRUE)) %>%
  mutate(region = "National") %>%
  bind_rows(reported_cases_regional) 

# Filter to those regions with more than 50 cumulative cases in the past two weeks
reported_cases_regional %>%
  filter(date > Sys.Date() - 7) %>%
  group_by(region) %>%
  summarize(confirm = sum(confirm)) %>%
  filter(confirm > 50) %>%
  .$region -> current

reported_cases_regional %<>% filter(region %in% current)

# Doing this for a subset cluster test
# reported_cases_regional %<>% filter(region %in% current[1:3])

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
system.time({
EpiNow::regional_rt_pipeline(
  cases = reported_cases_regional,
  delay_defs = delay_defs,
  incubation_defs = incubation_defs,
  target_folder = fp("output/rt_ests"),
  case_limit = 0,
  min_forecast_cases = 0,
  horizon = 14,
  approx_delay = TRUE,
  report_forecast = TRUE,
  forecast_model = function(y, ...){EpiSoon::forecastHybrid_model(
    y = y[max(1, length(y) - 21):length(y)],
    model_params = list(models = "aefz", weights = "equal"),
    forecast_params = list(PI.combination = "mean"), ...)})
})

# Then the summaries
# find the most recent date by getting the latest directory
tidyr::separate(
  tibble::tibble(
    name = list.files(fp("output/rt_ests/"), recursive = TRUE)), 
  name, "/", into = c("location", "date", "obj")) %$%
  max(date) -> targ_date

EpiNow::regional_summary(results_dir = fp("output/rt_ests"),
                         summary_dir = fp("output/rt_ests-summary"),
                         target_date = targ_date,
                         region_scale = "Region",
                         csv_region_label = "region",
                         log_cases = TRUE)


# Rt estimates for plotting
files <- list.files(fp("output/rt_ests"), recursive = TRUE, full.names = TRUE)
reff_files <- files[grep("latest/summarised_reff.rds", files)]
reffs <- lapply(reff_files, 
                function(x) {
                  out <- readRDS(x) 
                  out$region <- gsub(".*rt_ests/(.+)/latest.*", "\\1", x)
                  out$R0_range <- NULL
                  return(out)
                })
data.table::rbindlist(reffs) -> rt_ests
rt_ests <- rt_ests[region %in% current]
write_create(rt_ests, fp("latest/rt_ests.csv"), write_csv)

# Rt estimates for plotting
files <- list.files(fp("output/rt_ests"), recursive = TRUE, full.names = TRUE)
nowcast_files <- files[grep("latest/summarised_nowcast.rds", files)]
nowcast <- lapply(nowcast_files, 
                function(x) {
                  out <- readRDS(x) 
                  out$region <- gsub(".*rt_ests/(.+)/latest.*", "\\1", x)
                  return(out)
                })
data.table::rbindlist(nowcast) -> nowcast

# Rt estimates for plotting
files <- list.files(fp("output/rt_ests"), recursive = TRUE, full.names = TRUE)
forecast_files <- files[grep("latest/case_forecast.rds", files)]
forecast <- lapply(forecast_files, 
                  function(x) {
                    out <- readRDS(x) 
                    out$region <- gsub(".*rt_ests/(.+)/latest.*", "\\1", x)
                    return(out)
                  })
data.table::rbindlist(forecast) -> forecast

# Filter to current locations and also combine forecasts with nowcasts
forecast %>%
  filter(region %in% current) %>%
  mutate(type = rt_type) %>%
  select(-std, -range, -rt_type) %>%
  bind_rows(filter(nowcast, region %in% current)) -> case_cast

write_create(case_cast, fp("latest/case_cast.csv"), write_csv)

# Summary table
rt_summary <- readRDS(fp("output/rt_ests-summary/summary_table.rds"))
rt_summary %>%
  filter(Region %in% current) %>%
  select(-`New confirmed cases by infection date`) %>%
  mutate(date = readRDS(fp("output/rt_ests-summary/latest_date.rds")),
         date_ests = Sys.Date()) -> rt_summary
write_create(rt_summary, fp("latest/rt_summary.csv"), write_csv)

# Parse these from subutil for where to put things
syncto <- "~/Documents/Projects/to_archive/mada_rt/latest/"
syncfrom <- "mrajeev@della.princeton.edu:/scratch/gpfs/mrajeev/mada_rt/latest/"

# Close out
close_cl(cl)
print("done:)")