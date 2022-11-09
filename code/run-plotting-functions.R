#####################################################################
# Call plotting functions on the separate set of parameter choices
# that became available in late 2020 or beyond
# Done in response to reviewer Nov. 2022
#####################################################################

# Load libararies and plotting functions
library(rtZIKVrisk)
library(tidyverse)
source("code/covid-plotting-fxns.R")

fig_dir = "figures/inf_period_vary"
if(!dir.exists(fig_dir)){
  dir.create(fig_dir)
}

# Define data you want to plot/map/analyze
data_dir = "processed_data/inf_period_vary"
sys_date="2022-11-08" # "2022-11-04" for inf_period_4
r0_vect=c(1.1, 1.5, 3)
gen_time=c(5, 5.5, 6)
date="2020-03-16"
for(i in 1:length(r0_vect)){
  for(j in 1:length(gen_time)){
    path = paste0(data_dir, "/sim_", r0_vect[i], "_", gen_time[j], "_1e+05_", sys_date, ".rda")
    
    # Create epi_prob_data and cty_data
    save_cty_data(data_path = path, case_date = date) 
    
    # Load data created in save_cty_data and generate plot
    load(paste0(data_dir, "/county-summary_", 
                r0_vect[i], "_", gen_time[j], "_1e+05_", sys_date, ".rda")) # cty_data
    us_plot = plot_county_risk(county_data = cty_data)
    
    # Save plot based on inputs
    png(file=paste0(fig_dir, "/us_map_",  r0_vect[i], "_", gen_time[j], "_", date,  ".png" ),
        width=5.25,height=3.25, units = "in", res=1200)
    plot(us_plot)
    dev.off()
  } # end for j
} # end for i

# Get all the summary 
df = get_all_summary_data(folder_path = data_dir)

# Make 
gen_time %>% 
  map(.f = make_case_risk_plot, folder_path = data_dir, fig_path=fig_dir, r_not_vect=r0_vect, sys_date=sys_date)












