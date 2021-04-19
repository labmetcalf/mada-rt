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
library(EpiNow2)
library(data.table)
library(googlesheets4)
library(lubridate)
library(dplyr)

# delay distributions ----
reporting_delay <- estimate_delay(rlnorm(1000,  log(3), 1),
                                  max_value = 15, bootstraps = 1)
generation_time <- get_generation_time(disease = "SARS-CoV-2", source = "ganyani")
incubation_period <- get_incubation_period(disease = "SARS-CoV-2", source = "lauer")

# data -----
gs4_deauth()
data   <- read_sheet("https://docs.google.com/spreadsheets/d/1oQJl4HiTviKAAhCjMmg0ipGcO79cZg6gSHrdTuQID_w/edit#gid=0",
                     sheet = 9)

# Filter data to a max 4 month window (excluding the past 3 days)
# And starting from the date they began daily reporting again
date_min <- case_when(Sys.Date() - 4 * 30 > ymd("2021-03-01") ~ ymd(Sys.Date() - 4 * 30),
                      TRUE ~ ymd("2021-03-01"))
data %>%
  mutate(date = ymd(week)) %>%
  filter(date < Sys.Date() & date > date_min) %>% # just including today since these are date reports
  group_by(date, region) %>%
  summarise(confirm = sum(cases)) %>%
  tidyr::complete(date = seq.Date(min(date), max(date), by = "days"), 
                  fill = list(confirm = 0)) -> reported_cases_regional

reported_cases_regional %>%
  group_by(date) %>%
  summarize(confirm = sum(confirm, na.rm = TRUE)) -> reported_cases_natl

# Key outputs = nowcast, rt_ests, rt_summary

# Run national ----
system.time({
  estimates <- epinow(reported_cases = reported_cases_natl, 
                      generation_time = generation_time,
                      delays = delay_opts(incubation_period, reporting_delay),
                      stan = stan_opts(samples = 4000, 
                                       warmup = 2000,
                                       cores = 3))
})
save_example <- plot(estimates)
ggsave("EpiNow2_exe_natl.jpeg", save_example)
agp <- estimate_infections(reported_cases = reported_cases_natl, 
                           generation_time = generation_time,
                           delays = delay_opts(incubation_period, reporting_delay),
                           rt = rt_opts(prior = list(mean = 2, sd = 0.1)),
                           gp = gp_opts(ls_min = 10, basis_prop = 0.1),
                           stan = stan_opts(control = list(adapt_delta = 0.95)),
                           CrIs = c(0.5, 0.75))

backcalc <- estimate_infections(reported_cases = reported_cases_natl, 
                                generation_time = generation_time,
                                delays = delay_opts(incubation_period, reporting_delay),
                                rt = NULL, backcalc = backcalc_opts(prior = "none"),
                                obs = obs_opts(scale = list(mean = 0.4, sd = 0.05)),
                                horizon = 7)
plot(backcalc)

# Rt projected into the future using the Gaussian process
project_rt <- estimate_infections(reported_cases = reported_cases_natl, generation_time = generation_time,
                                  delays = delay_opts(incubation_period, reporting_delay),
                                  rt = rt_opts(prior = list(mean = 2, sd = 0.1), 
                                               future = "project"))
plot(project_rt)


