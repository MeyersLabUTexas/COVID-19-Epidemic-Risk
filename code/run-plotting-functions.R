#####################################################################
# Call plotting functions on the separate set of parameter choices
# that became available in late 2020 or beyond
# Done in response to reviewer Nov. 2022
#####################################################################

# Load libararies and plotting functions
library(rtZIKVrisk)
library(tidyverse)
source("code/covid-plotting-fxns.R")

fig_dir = "figures/inf_period_6"
if(!dir.exists(fig_dir)){
  dir.create(fig_dir)
}

# Define data you want to plot/map/analyze
data_dir = "processed_data/inf_period_6"
file_list = list.files(data_dir, pattern="^sim_")
sim_params = file_list %>%
  as_tibble() %>%
  separate(value, into = c("sim", "r_not", "gen_time", "n_sim", "sys_date"), sep = "_" ) %>%
  mutate(sys_date = str_sub(sys_date, end = -5) ) %>%
  select(-sim)
r0_vect = unique(sim_params$r_not)
gen_time= unique(sim_params$gen_time)
num_sims= unique(sim_params$n_sim)
sys_date= unique(sim_params$sys_date)
cty_date_vect=c("2020-03-16", "2020-03-23", "2020-04-13") # date to get cumulative cases

R0_round_counties_all_dates = data.frame()
urb_rural_R0_counties_all_dates = data.frame()
for(j in 2:3){ #1:length(cty_date_vect)
  all_cty_data = data.frame() 
  for(i in 1:length(r0_vect)){
    path = paste0(data_dir, "/", file_list[i])
    
    # Create epi_prob_data and cty_data
    save_cty_data(data_path = path, case_date = cty_date_vect[j])
    
    # Load data created in save_cty_data and generate plot
    load(paste0(data_dir, "/county-summary_",
                sim_params$r_not[i], "_", sim_params$gen_time[i], "_",
                sim_params$n_sim[i], "_", sys_date, ".rda")) # cty_data
    cty_data_w_params = cty_data %>% # Save params to county data
      mutate(r_not = sim_params$r_not[i],
             model_parm_set = "alternate",
             date=cty_date_vect[j])
    all_cty_data = rbind(all_cty_data, cty_data_w_params)
  } # end for i
  
  ## Save data for each date in a csv
  write.csv(all_cty_data, paste0(data_dir, "/", cty_date_vect[j], "_county-risk-estimates-all-r0.csv"), 
            row.names = F)
  
  ## Select county epi_prob based on R0 of either rounded counties R0 estimate or urban/rural designation
  R0_round_counties = read_csv("processed_data/county_specific_R0.csv") %>%
    left_join(all_cty_data %>% rename( R0_round = r_not) %>% mutate(R0_round = as.double(R0_round)), 
              by=c("fips", "R0_round"))
  write.csv(R0_round_counties, paste0(data_dir, "/", cty_date_vect[j],"_R0_round_counties.csv"), 
            row.names = F)
  R0_round_counties_all_dates = rbind(R0_round_counties_all_dates, R0_round_counties)
  
  # Save plot based on inputs
  us_plot_r0_round = plot_county_risk(county_data = R0_round_counties)
  png(file=paste0(fig_dir, "/us_map_R0_round_counties_", cty_date_vect[j], ".png"),
      width=5.25,height=3.25, units = "in", res=1200)
  plot(us_plot_r0_round)
  dev.off()
  
  # Urban-rural 
  urb_rural_R0_counties = read_csv("processed_data/county_specific_R0.csv") %>%
    left_join(all_cty_data %>% rename(mean_R0_spr_grp_round = r_not) %>% 
                mutate(mean_R0_spr_grp_round = as.double(mean_R0_spr_grp_round)), 
              by=c("fips", "mean_R0_spr_grp_round"))
  write.csv(urb_rural_R0_counties, paste0(data_dir, "/", cty_date_vect[j], "_urb_rural_R0_counties.csv"), 
            row.names = F)
  urb_rural_R0_counties_all_dates = rbind(urb_rural_R0_counties_all_dates, urb_rural_R0_counties)
  
  # Save plot based on inputs
  us_plot_r0_urb_rural = plot_county_risk(county_data = urb_rural_R0_counties)
  png(file=paste0(fig_dir, "/us_map_R0_urb_rural_counties_", cty_date_vect[j], ".png"),
      width=5.25,height=3.25, units = "in", res=1200)
  plot(us_plot_r0_urb_rural)
  dev.off()
} # end for j

# Write epi_prob for all dates and counties to file
R0_round_counties_all_dates = R0_round_counties_all_dates %>%
  rename(R0_round_epi_prob = epi_prob)
write.csv(R0_round_counties_all_dates, paste0(data_dir, "/", "all_dates_R0_round_counties.csv"), 
          row.names = F)

r0_diff = urb_rural_R0_counties_all_dates %>%
  rename(urb_rural_R0_epi_prob = epi_prob) %>%
  left_join(R0_round_counties_all_dates %>% select(fips, date, R0_round_epi_prob), by=c("fips", "date")) %>%
  mutate(R0_diff = R0_round - mean_R0_spr_grp_round,
         epi_prob_diff = R0_round_epi_prob - urb_rural_R0_epi_prob)
  
write.csv(r0_diff, paste0(data_dir, "/", "all_dates_R0_compare_counties.csv"), 
          row.names = F)

## Compare R0 differences across dates
r0_compare = ggplot(r0_diff %>% drop_na(date), aes(x=R0_diff, epi_prob_diff, color=date))+
  geom_point(alpha=0.25)+
  facet_wrap(~date)+
  theme_bw()
png(file=paste0(fig_dir, "/epi_prob_compare.png"),
    width=7.25,height=5.25, units = "in", res=1200)
plot(r0_compare)
dev.off()

# Get all the summary 
df = get_all_summary_data(folder_path = data_dir)

# Make 

min(r0_diff$R0_round, na.rm = T)
max(r0_diff$R0_round, na.rm = T)
median(unique(r0_diff$R0_round), na.rm = T)

# need to add Travis County March 13 and 20 line
make_case_risk_plot(folder_path = data_dir, fig_path=fig_dir, r_not_vect=r0_vect[2:], 
                    sys_date=sys_date, gen_time = gen_time)












