###################################################################################################
# Get the R0 estimate for each county based on https://www.nature.com/articles/s42003-020-01609-6
# Combined Option: Round each R0, this will not allow for the assignment of Alaska and Hawaii
# Then for only Alaska and Hawaii use the mean of their 6 urban-rural designations as estimate
###################################################################################################

get_county_specific_R0 = function(){
  # Does not have Alaska and Hawaii
  county_r0 = read_csv("raw_data/42003_2020_1609_MOESM4_ESM.csv") %>% 
    mutate(fips = str_pad(as.character(fips), 5, side = "left", pad = "0"))
  
  # Urban-rural from https://www.cdc.gov/nchs/data_access/urban_rural.htm#Data_Files_and_Documentation
  county_desig = readxl::read_xlsx("raw_data/NCHSURCodes2013.xlsx") %>%
    rename(fips = `FIPS code`) %>%
    mutate(fips = str_pad(as.character(fips), 5, side = "left", pad = "0"))

  load("processed_data/original_data/county-summary_1.1_0.1_0_1e+05.rda") # load cty_data
  # These locations appear in the county_desig but are not in original analysis of 3,142 counties
  # fips  `State Abr.` `County name`                     
  # 02201  AK           Prince of Wales-Outer Ket        
  # 02232  AK           Skagway-Hoonah-Angoon Census Area 
  # 02270  AK           Wade Hampton Census Area          
  # 02280  AK           Wrangell-Petersburg Census Area   
  # 46113  SD           Shannon County                   
  # 51515  VA           Bedford city 
  # 51560  VA           Clifton Forge city                
  
  county_join = county_desig %>%
    filter(fips %in% cty_data$fips) %>% # get only the 3,142 counties from real-time analysis
    left_join(county_r0, by="fips") %>%
    mutate(R0_round_missing = round(R0.pred, 1)) %>%
    group_by(`2013 code`) %>%
    mutate(mean_R0_2013_code_round = round(mean(R0.pred, na.rm=T), 1),
           lb_R0_2013_code_round = round(quantile(R0.pred, probs = 0.025, na.rm=T), 1),
           ub_R0_2013_code_round = round(quantile(R0.pred, probs = 0.975, na.rm=T), 1) ) %>%
    select(fips, R0.pred, R0_round_missing, `2013 code`, 
           mean_R0_2013_code_round, lb_R0_2013_code_round, ub_R0_2013_code_round) %>%
    mutate(R0_round = ifelse(is.na(R0_round_missing), mean_R0_2013_code_round, R0_round_missing))
  
  write.csv(county_join, "processed_data/county_specific_R0.csv", row.names = F)
  
  return("county_specific_R0.csv created")
} # end function








