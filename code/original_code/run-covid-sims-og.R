###############################################
# Code for ORIGINAL set of parameter choices
# these were the best estimates in March 2020
# Maps as seen in the Apr 3 2020 NYT article
###############################################

## Load libraries
library(tidyverse)
library(rtZIKVrisk)

## Source functions
an.error.occured1 <- FALSE 
tryCatch( { result <- file.exists("code/sim-covid-outbreaks.R"); print(result) },
          warning = function(w) { print("Can't find the file you're looking for") },
          error = function(e) {an.error.occured1 <<- TRUE})
stopifnot(an.error.occured1==FALSE)
source("code/original_code/sim-covid-outbreaks.R")

an.error.occured2 <- FALSE 
tryCatch( { result <- file.exists("code/covid-plotting-fxns.R"); print(result) },
          warning = function(w) { print("Can't find the file you're looking for") },
          error = function(e) {an.error.occured2 <<- TRUE})
stopifnot(an.error.occured2==FALSE)
source("code/original_code/covid-plotting-fxns-og.R")

## Setup all of the parameters that need to be run
r_not <- c(1.1, 1.5, 3)
detection_probability <- c(0.05, 0.1, 0.2, 0.3, 0.4)
importation_rate <- c(0)
num_runs=100000
run_df <- expand_grid(r_not, detection_probability, importation_rate)

## Run and save simulations across all parameter combinations
dir_path = "processed_data/original_data"
if(!dir.exists(dir_path)){
  dir.create(dir_path)
}
run_df %>% 
  pmap(.f = save_covid_runs, num_reps = num_runs) %>% 
  unlist()

## Get all simulation county data pre-processed
# need to run to update counties with most recent case data
# Going to compare 2020-03-16 and 2020-04-13, then  2020-03-16 and 2020-03-23 in retrospective
cty_date = "2020-03-16" # supplying specific date instead of most recent in NYT data set
run_df %>% 
  pmap(.f = get_save_path, num_reps = num_runs, dir_path=dir_path) %>% 
  map(save_cty_data, case_date = cty_date) 

## Create maps for figures
# load specific data set to build maps with
load(get_save_path(r_not = 1.5, 
                   detection_probability = 0.1, 
                   importation_rate = 0,
                   num_reps = num_runs, 
                   dir_path = dir_path,
                   summary = TRUE))

## Plot the various figures
fig_path = "figures/original_data"
if(!dir.exists(fig_path)){
  dir.create(fig_path)
}
if(is.na(cty_date)){
  cty_date=unique(cty_data$date)
  cty_date=cty_date[!is.na(cty_date)]
}

plot_county_risk(cty_data) %>% 
  save_plot(plot = ., filename = paste0(fig_path, "/us_baseline_risk_map", cty_date[1], ".png"), base_height = 4, base_aspect_ratio = 1.7)
plot_county_risk(cty_data, state = "Texas") %>% 
  save_plot(plot = ., filename = paste0(fig_path, "/tx_baseline_risk_map", cty_date[1], ".png"), base_height = 4, base_aspect_ratio = 1.3)

## Save data for each date in a csv
write_csv(cty_data, paste0(dir_path, "/", cty_date, "county-risk-estimates.csv"))

## Plot case summary statistics
get_all_summary_data(dir_path) %>% 
  plot_county_summary_sensitivity() %>% 
  save_plot(plot = ., filename = paste0(fig_path, "/us_sensitivity_plot.png"), 
            base_height = 4, base_aspect_ratio = 1.5)

## Plot cases by epidemic risk for all R0
make_case_risk_plot(r_not_vect=r_not, det_prob=0.1, dir_path, fig_path)

## Get case summary statistics
get_summary_stats(cty_data, threshold=0.9)
  
## Number of county remaining with less than 5 detected cases  
sub=subset(cty_data, cases<5)
length(sub$county)


