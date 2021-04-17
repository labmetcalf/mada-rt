# From https://github.com/epiforecasts/covid-rt-estimates/blob/master/R/update-delays.R

# Packages -----
require(EpiNow2)
require(covidregionaldata)
require(data.table)
require(future)
require(lubridate)


# Set up parallel -----
if (!interactive()) {
  # If running as a script enable this
  options(future.fork.enable = TRUE)
}

plan(multiprocess)

# Fit delay from onset to admission ----
report_delay <- covidregionaldata::get_linelist(report_delay_only = TRUE)
report_delay <- data.table::as.data.table(report_delay)[!(country %in% c("Mexico", "Phillipines"))]

onset_to_admission_delay <- EpiNow2::bootstrapped_dist_fit(report_delay$days_onset_to_report, bootstraps = 100, 
                                                           bootstrap_samples = 250, max_value = 30)

saveRDS(onset_to_admission_delay, here::here("data", "onset_to_admission_delay.rds"))
