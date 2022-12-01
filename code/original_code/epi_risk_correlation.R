#####################################################################
# Make one large data frame of Real-time/Original with Retrospective
#  used in Fig 4
# Correlate March 16 and 23 epidemic risk 
#####################################################################

# Get real-time estimates of epi-risk
load("processed_data/original_data/2020-03-16county-risk-estimates-all-r0.rda")
d1=all_cty_data
load("processed_data/original_data/2020-03-23county-risk-estimates-all-r0.rda")
d2 = all_cty_data
all_date_og=rbind(d1, d2)
r0_all_og = all_date_og %>%
  #filter(r_not==1.5) %>%
  st_drop_geometry() %>% # select won't remove the geometry
  select(-moe, -detection_probability, -importation_rate, -variable, -estimate)

# Get retrospective estimates of epi-risk per R0
load("processed_data/inf_period_6/all_dates_R0_round_counties.rda")

# Get only the 2 validation dates R0 and cases for retrospective model
all_date_retro = R0_round_counties_all_dates %>%
  filter(date %in% c("2020-03-16", "2020-03-23")) %>%
  rename(r_not = R0_round) %>%
  select(names(r0_all_og))

# Make into one big df # Used in Fig 4
full_date_df = rbind(r0_all_og, all_date_retro)
write.csv(full_date_df, "processed_data/inf_period_6/validation_date_model_compare.csv", row.names = F)

##################################################
# Correlation of March 16 and 23 for original data
##################################################
mar16 = r0_all_og %>% 
  filter(r_not==1.5) %>%
  filter(date=="2020-03-16")
mar23 = r0_all_og %>% 
  filter(r_not==1.5) %>%
  filter(date=="2020-03-23")

cor.test(mar16$epi_prob, mar23$epi_prob)
