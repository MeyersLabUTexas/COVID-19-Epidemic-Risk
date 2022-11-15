###################################################################################################
# Get the R0 estimate for each county based on https://www.nature.com/articles/s42003-020-01609-6
# Option 1: Urban-rural designation of each county to get the mean and 95%CI of R0
# Option 2: Round each R0, this will not allow for the assignment of Alaska and Hawaii
###################################################################################################

# Does not have Alaska and Hawaii
county_r0 = read_csv("raw_data/42003_2020_1609_MOESM4_ESM.csv") %>% 
  mutate(fips = str_pad(as.character(fips), 5, side = "left", pad = "0"))

# Urban- rural from https://www.cdc.gov/nchs/data_access/urban_rural.htm#Data_Files_and_Documentation
county_desig = readxl::read_xlsx("raw_data/NCHSURCodes2013.xlsx") %>%
  rename(fips = `FIPS code`) %>%
  mutate(fips = str_pad(as.character(fips), 5, side = "left", pad = "0"))

# 2019 County population, not needed unless making commented out plot below
# county_pop = read_csv("raw_data/county_pop_2019.csv") %>% 
#   mutate(fips = str_pad(as.character(fips), 5, side = "left", pad = "0"))

county_join = county_desig %>%
  left_join(county_r0, by="fips") %>%
  #left_join(county_pop, by="fips") %>%
  mutate(super_grp = ifelse((`2013 code`==1 | `2013 code`==2), "Large Metro", 
                               ifelse((`2013 code`==3 | `2013 code`==4), "Med-small Metro", "Rural Nonmetro") ),
         R0_round = round(R0.pred, 1)) %>%
  group_by(super_grp) %>%
  mutate(mean_R0_spr_grp = mean(R0.pred, na.rm=T ),
         lb_R0_spr_grp = quantile(R0.pred, probs = 0.025, na.rm=T),
         ub_R0_spr_grp = quantile(R0.pred, probs = 0.975, na.rm=T),
         mean_R0_spr_grp_round = round(mean_R0_spr_grp, 1),
         lb_R0_spr_grp_round = round(lb_R0_spr_grp, 1),
         ub_R0_spr_grp_round = round(ub_R0_spr_grp, 1) ) %>%
  select(fips, R0.pred, R0_round, `2013 code`, super_grp, mean_R0_spr_grp, lb_R0_spr_grp, ub_R0_spr_grp,
         mean_R0_spr_grp_round, lb_R0_spr_grp_round, ub_R0_spr_grp_round)
  
write.csv(county_join, "processed_data/county_specific_R0.csv", row.names = F)


ggplot(county_join, aes(x=R0.pred, group=as.factor(super_groups), fill=as.factor(super_groups) ))+
  geom_density(alpha=0.35)+
  theme_bw()

# ggplot(county_join, aes(y=log(POPESTIMATE2019), x=R0.pred, color=as.factor(`2013 code`) ) )+
#   geom_point(alpha=0.5)+
#  # geom_smooth(y~x)+
#   theme_bw()







