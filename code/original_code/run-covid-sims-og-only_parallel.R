##############################################################
## Real-time code for running and saving all COVID simulations
##############################################################
# Run from inside the code dir

library(tidyverse)
# plyr retired and no longer part of tidyverse, now dplyr or purrr

## Check file with list of parameters exists
an.error.occured1 <- FALSE 
tryCatch( { result <- file.exists("original_code/sim-covid-outbreaks.R"); print(result) },
          warning = function(w) { print("Can't find the file you're looking for") },
          error = function(e) {an.error.occured1 <<- TRUE})
stopifnot(an.error.occured1==FALSE)
source("original_code/sim-covid-outbreaks.R")
print("og sim file found/added")

## Setup all of the parameters that need to be run
args             = commandArgs(TRUE)
r_not            = as.double(args[1]) # R0 = 1.1, 1.5, 3
gen_interval     = as.double(args[2]) # gen_time = 5, 5.5, 6
run_df           = expand_grid(r_not, gen_interval)
num_runs         = 100000

## Run and save simulations across all parameter combinations
output_path = "../processed_data/original_params_test"
if(!dir.exists(output_path)){
  dir.create(output_path)
}
run_df %>% # pipe the 4 inputs into save_covid_runs function, when refresh is FALSE it will not overwrite existing output
  pmap(.f = save_covid_runs, num_reps = num_runs, refresh=TRUE, dir_path=output_path) %>% 
  unlist()
