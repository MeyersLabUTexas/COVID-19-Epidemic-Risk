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

# Only need to run these lines of code if you need to run all the simulations for first time
# run_df %>% 
#   pmap(.f = save_covid_runs, num_reps = num_runs) %>% 
#   unlist()

## Get all simulation county data pre-processed
# need to run to update counties with most recent case data
# Going to compare 2020-03-16 and 2020-04-13, then  2020-03-16 and 2020-03-23 in retrospective
cty_date = "2020-03-16" # supplying specific date instead of most recent in NYT data set
run_df %>% 
  pmap(.f = get_save_path, num_reps = num_runs, dir_path=dir_path) %>% 
  map(save_cty_data, case_date = cty_date) 

## Create maps for figures
fig_path = "figures/original_data"
if(!dir.exists(fig_path)){
  dir.create(fig_path)
}
if(is.na(cty_date)){
  cty_date=unique(cty_data$date)
  cty_date=cty_date[!is.na(cty_date)]
}


# Loop over all Re/R0 to make maps and save each epi_prob as col in csv
all_cty_data = data.frame()
for(i in 1:length(r_not)){
  # load specific data set to build maps with
  print(paste0("Date ", cty_date, "and R0=", r_not[i]))
  load(get_save_path(r_not = r_not[i], # 1.1, 1.5, 3
                     detection_probability = detection_probability[2], # 0.1
                     importation_rate = importation_rate[1], # 0
                     num_reps = num_runs, 
                     dir_path = dir_path,
                     summary = TRUE))
  cty_data = cty_data %>%
    mutate(date = cty_date)
  
  ## Plot the various figures
  png(file=paste0(fig_path, "/us_baseline_risk_map_", cty_date[1], "_", r_not[i], "_og_params.png"),
      width=5.75,height=3.25, units = "in", res=1200)
  print(plot_county_risk(cty_data))
  dev.off()
  print("plot finished")
  
  # Add params to the df
  cty_data_w_params = cty_data %>%
    mutate(r_not = r_not[i],
           detection_probability = detection_probability[2],
           importation_rate = importation_rate[1], 
           model_parm_set = "original")
  
  all_cty_data = rbind(all_cty_data, cty_data_w_params)
  print("Data appended")
} # end for i

## Save data for each date in a csv
save(all_cty_data, file=paste0(dir_path, "/", cty_date, "county-risk-estimates-all-r0_", cty_date,".rda"))
#write_csv(cty_data, paste0(dir_path, "/", cty_date, "county-risk-estimates.csv"))

compare_table = all_cty_data %>%
  select(r_not, detection_probability,  importation_rate, model_parm_set, date, cases, epi_prob) %>%
  group_by(r_not, cases) %>%
  slice(1) %>%
  ungroup() %>%
  filter(cases %in% c(0, 1)) %>%
  mutate(epi_perc = epi_prob*100)


## Plot sensitivity about detection probability
df  = get_all_summary_data(dir_path)

plot_county_summary_sensitivity(df) %>% 
  save_plot(plot = ., filename = paste0(fig_path, "/new_us_sensitivity_plot_", cty_date, ".png"), 
            base_height = 4, base_aspect_ratio = 1.75)

## Plot cases by epidemic risk for all R0
make_case_risk_plot(r_not_vect=r_not, det_prob=0.1, dir_path, fig_path)

## Get case summary statistics
get_summary_stats(all_cty_data %>% filter(r_not == 1.5), threshold=0.9)
  
## Number of county remaining with less than 5 detected cases  
sub=subset(all_cty_data %>% filter(r_not == 1.5), cases<5)
length(sub$county)

## 












