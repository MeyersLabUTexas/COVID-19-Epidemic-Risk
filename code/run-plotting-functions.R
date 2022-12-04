#####################################################################
# Call plotting functions on the separate set of parameter choices
# "retrospective model" as oppose to code original "real-time" model
# Done in response to reviewers Dec.10.2022
#####################################################################

# Load libararies and plotting functions
library(rtZIKVrisk)
library(tidyverse)
library(cowplot)
source("code/covid-plotting-fxns.R")

# Get county specific R0 from pre-lockdowns early 2020
if(!file.exists("processed_data/county_specific_R0.csv")){
  source("code/get_county_specific_R0.R")
  get_county_specific_R0()
}

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
plot_list = rep(list(NA), length(cty_date_vect))
for(j in 1:length(cty_date_vect)){
  outfile = paste0(data_dir, "/", cty_date_vect[j], "_county-risk-estimates-all-r0.rda")
  if(!file.exists(outfile)){
    all_cty_data = data.frame() 
    for(i in 1:length(r0_vect)){
      print( paste0("Date=", cty_date_vect[j], " R0=", r0_vect[i]) )
      path = paste0(data_dir, "/", file_list[i])
      
      # Create epi_prob_data and cty_data
      save_cty_data(data_path = path, case_date = cty_date_vect[j])
      
      # Load data created in save_cty_data and generate plot
      load(paste0(data_dir, "/county-summary_",
                  sim_params$r_not[i], "_", sim_params$gen_time[i], "_",
                  sim_params$n_sim[i], "_", sys_date, ".rda")) # cty_data
      cty_data_w_params = cty_data %>% # Save params to county data
        mutate(r_not = sim_params$r_not[i],
               model_parm_set = "retrospective",
               date=cty_date_vect[j])
      all_cty_data = rbind(all_cty_data, cty_data_w_params)
    } # end for i
    
    ## Save data for each date in a rda, this is a big unfiltered file
    save(all_cty_data, file=paste0(data_dir, "/", cty_date_vect[j], "_county-risk-estimates-all-r0.rda"))
  }else{
    load(outfile) # loads all_cty_data
  }
  
  ## Select county epi_prob based on R0 of either rounded counties R0 estimate or urban/rural designation
  R0_round_counties = read_csv("processed_data/county_specific_R0.csv", col_types = c("fips"="c")) %>%
    mutate(fips = str_pad(as.character(fips), 5, side = "left", pad = "0")) %>% # 3,142 counties
    left_join(all_cty_data %>% rename(R0_round = r_not) %>% mutate(R0_round = as.double(R0_round)), 
              by=c("fips", "R0_round")) #%>%
    #drop_na() # this was dropping the na of R0.pred which is all Alaska and Hawaii
  save(R0_round_counties, file = paste0(data_dir, "/", cty_date_vect[j],"_R0_round_counties.rda"))
  
  R0_round_counties_all_dates = rbind(R0_round_counties_all_dates, R0_round_counties)
  
  ## Fig 1
  # Save plot based on inputs
  us_plot_r0_round = plot_county_risk(county_data = R0_round_counties)
  png(file=paste0(fig_dir, "/us_map_R0_round_counties_", cty_date_vect[j], ".png"),
      width=5.25,height=3.25, units = "in", res=1200)
  plot(us_plot_r0_round)
  dev.off()
  
  # save plot to list without legend
  plot_list[[j]] = ggplotGrob(us_plot_r0_round+theme(legend.position="none"))
} # end for j

# Write epi_prob for all dates and counties to file
save(R0_round_counties_all_dates, file = paste0(data_dir, "/", "all_dates_R0_round_counties.rda"))

## Combine plots
# These don't look good so I combined plots in a separate app
legend = get_legend(us_plot_r0_round)
plot_grid(plot_list[[1]], plot_list[[3]], legend, rel_heights = c(1, 1, .1), nrow=1)

## Summary stats
# load(file = paste0(data_dir, "/", "all_dates_R0_round_counties.rda"))
epi_prob_date_case = R0_round_counties_all_dates %>%
  group_by(date, cases) %>%
  summarise(min_epi = min(epi_prob),
            max_epi = max(epi_prob),
            mean_epi = mean(epi_prob),
            lb = quantile(epi_prob, probs = 0.025),
            ub = quantile(epi_prob, probs = 0.975))

R0_round_counties_all_dates %>%
  group_by(date) %>%
  summarise(med_epi = median(epi_prob),
            mean_epi = mean(epi_prob),
            lb = quantile(epi_prob, probs = 0.025),
            ub = quantile(epi_prob, probs = 0.975))

R0_round_counties_all_dates %>%
  group_by(date) %>%
  summarise(total_county = n(),
            total_pop = sum(population),
            perc_50 = round((sum(epi_prob>0.5)/total_county)*100, 0),
            per_pop_50 = round((sum(population[epi_prob>0.5])/total_pop)*100, 0),
            perc_90 = round((sum(epi_prob>0.9)/total_county)*100, 0),
            per_pop_90 = round((sum(population[epi_prob>0.9])/total_pop)*100, 0) ) #,
            # perc_case_1 = round(sum(cases>=1)/total_county*100, 0),
            # perc_pop_1 = round(sum(population[cases>=1])/total_pop*100, 0) )

# Get total deaths in 2020
total_death_2020 = read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") %>%
  filter(date =="2020-12-31") %>%
  filter(fips %in% R0_round_counties_all_dates$fips) %>% # missing counties have 0 cases reported
  summarize(all_case = sum(cases),
            all_death = sum(deaths))



## Fig 2
# Compare epi risk across 4 R0: min, median, max, and 1.1 which may have occurred after lock-down
min_r0 = min(R0_round_counties$R0_round, na.rm = T)
max_r0 = max(R0_round_counties$R0_round, na.rm = T)
med_r0 = median(unique(R0_round_counties$R0_round), na.rm = T)
travis_r0 = R0_round_counties$R0_round[R0_round_counties$fips=="48453"] # 2

# need to add Travis County March 13 and 20 line
make_case_risk_plot(folder_path=data_dir, fig_path=fig_dir, r_not_vect=c(1.1, min_r0, med_r0, max_r0),
                    sys_date=sys_date, gen_time=gen_time)


## Fig 3
# weeks_to_1000_cases_plot.R








