###############################
## Code for plotting
###############################
save_cty_data <- function(data_path, case_date = NULL){
  cty_data <- get_cty_data(data_path, case_date)
  save("cty_data", file = str_replace(string = data_path, pattern = "sim", replacement = "county-summary"))
}

get_cty_data <- function(data_path, case_date = NULL){
  require(usmap)
  require(scam)

  load(data_path)
  epi_probs <- rtZIKVrisk::get_epidemic_prob_by_d(trials = sims, 
                                      prev_threshold = 50,
                                      cum_threshold = 2000, # should match epi_thresh
                                      max_detect = 210) # the max number of cases to get epi_prob for
  
  # Ensure epidemic probabilities are monotonic increasing by fitting scam to data
  epi_probs$prob_epidemic[is.na(epi_probs$prob_epidemic)]=1
  scam_convert=scam(formula = epi_probs$prob_epidemic ~ s(epi_probs$detected, k = 25, bs = "micv"))
  scam_prob_epidemic = scam_convert$fitted.values
  scam_prob_epidemic[scam_prob_epidemic > 1]=1
  
  # write to processed_data folder to use for plotting
  epi_case_df = data.frame(epi_probs$detected, epi_probs$prob_epidemic, scam_prob_epidemic)
  names(epi_case_df)=c("detected", "prob_epidemic", "scam_prob_epidemic")
  csv_path=str_replace(string = data_path, pattern = "rda", replacement = "csv")
  epi_path=str_replace(string = csv_path, pattern = "sim", replacement = "epi_prob_data")
  write.csv(epi_case_df, epi_path, row.names = FALSE)
  
  ## Read in case data and subset to correct date
  ## Defaults to using the most recent case data
  cty_case_data <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
  if(is.null(case_date)){
    cty_case_data <- cty_case_data %>% 
      arrange(desc(date)) %>% 
      group_by(fips) %>% 
      slice(1) %>% 
      ungroup()
  } else {
    cty_case_data <- cty_case_data %>% 
      filter(date == case_date)
  }
  
  ## Read in population data
  pop_data <- read_csv("raw_data/county_pop_2019.csv")
  
  ## Read in county shapefile data and attach all county information
  # usmap::us_map(regions = "counties")  %>%
  #   as_tibble() %>%
  #   distinct(fips, full) %>%
  shp_file = tidycensus::get_acs(geography="county", variables=c("B01001_001"), 
                                      geometry=TRUE, year=2019, shift_geo = TRUE) %>%
    separate(NAME, into = c("county", "state"), sep = ", ") %>%
    rename(fips=GEOID) %>%
    #filter(!(state=="Puerto Rico")) %>%
    inner_join(pop_data %>%
                 mutate(fips = str_pad(as.character(fips), 5, side = "left", pad = "0")), by = "fips") %>%
    rename(population = POPESTIMATE2019) %>% # , county = CTYNAME
    select(-CTYNAME) %>%
    left_join(cty_case_data %>% 
                select(fips, date, cases, deaths), by = "fips") %>% 
    mutate(cases = ifelse(is.na(cases), 0, cases),
           epi_prob = epi_probs$prob_epidemic[cases+1]) %>% 
           #epi_prob = scam_prob_epidemic[cases+1]) %>%
    mutate(epi_prob = ifelse(is.na(epi_prob), 1, epi_prob))
  
  return(shp_file)
}


### Fig 1
plot_county_risk <- function(county_data, state = NULL){
  
  # Cannot use us_map since it does not allow year specification
  # Alaska split a county that did not receive population data until 2021-2022 (after 2020 census)
  # shp_file <- usmap::us_map(regions = "counties")
  if(!is.null(state)){
    county_data <- county_data %>% filter(state == state)
  }
  
  break_min = min(county_data$epi_prob)*100
  break_min_lab = round(break_min, 0)
  
  require(sf)
  p1 = ggplot(county_data)+
    geom_sf(mapping=aes(geometry=geometry, fill=epi_prob*100), size = 0.05, color="black")+
    #geom_polygon(aes(group = group, fill = epi_prob*100), color = "black", size = 0.1) +
    scale_fill_gradient(low = "gainsboro", high = "dark red", name = "Epidemic Risk (%)",
                        #breaks=c(break_min, 50, 100), 
                        #labels=c(break_min_lab, 50, 100), limits=c(break_min,100)
                        breaks=c(0, 25, 50, 75, 100), 
                        labels=c(0, 25, 50, 75, 100), limits=c(0,100) )+
    scale_x_continuous("", breaks = NULL) + scale_y_continuous("", breaks = NULL)+
    theme_void()+
    theme(
      legend.position = "right",
      legend.title=element_text(size=10),
      legend.text=element_text(size=8),
      legend.text.align = 1,
      #legend.key.size = unit(0.5, "lines"),
      #plot.margin=unit(c(0, 0, 0, 0),"cm"),
      panel.background  = element_rect(color = "white", fill = "white"),
      legend.background = element_rect(color = "white", fill = "white"))+
    guides(fill = guide_colourbar(ticks.colour='black', ticks.linewidth = 0.25, # barheight = unit(2, "in"), 
                                  border=element_line(color='black')))
  
    return(p1)
} # end plot_county_risk

get_summary_stats <- function(county_df, state_stats = "Texas", threshold=0.5){
  county_df %>% 
    summarize(frac_us_population = sum(population[which(epi_prob>threshold)])/sum(population),
              frac_us_counties = sum(epi_prob>threshold)/n(),
              frac_state_population = sum(population[which(epi_prob>threshold & state == state_stats)]) /
                sum(population[which(state == state_stats)]),
              frac_state_counties = sum(epi_prob>threshold & state == state_stats)/sum(state == state_stats),
              state = state_stats)
}

get_all_summary_data <- function(folder_path){
  summary_files <- list.files(path = folder_path, full.names = T)
  summary_files[grepl(pattern = "county-summary", summary_files)] %>% 
    map( function(path){load(path); return(cty_data);}) %>% 
    map(get_summary_stats, state_stats = "Texas") %>% 
    bind_rows() %>% 
    mutate(path = summary_files[grepl(pattern = "county-summary", summary_files)]) %>% 
    separate(col = path, into = c("junk", "junk2", "keep"), sep = "\\/") %>% 
    separate(col = keep, into = c("junk3", "r_not", "detection_probability", "importation_rate", "num_reps"), sep = "_") %>%
    mutate(num_reps = str_sub(num_reps, end=-5)) %>% # remove the .rda
    select(-starts_with("junk"))
}

### Fig S1
plot_county_summary_sensitivity <- function(df){
  df %>% 
    select(-frac_state_counties, - frac_state_population) %>% 
    gather(key, value, frac_us_population, frac_us_counties) %>% 
    mutate(key = ifelse(key == 'frac_us_population', "US Population", "US Counties")) %>% 
    ggplot(aes(detection_probability, value, color = as.factor(r_not), group = r_not, shape=as.factor(r_not)))+
    geom_line(size=1) + 
    geom_point(size=2) +
    scale_y_continuous(labels = scales::percent, limits = c(0,1))+
    facet_wrap(~key) +
    background_grid(major = 'xy')+
    labs(color  = expression(R[e]), shape=expression(R[e]), linetype=expression(R[e]))+
    xlab("Case Detection Probability")+
    ylab("Percent")+
    scale_color_manual(values=c("#999999", "grey39", "#000000"))+
    theme_bw(base_size = 10)
}


### Fig 2
make_case_risk_plot=function(r_not_vect, det_prob, dir_path, fig_path, 
                             county_ex="Travis", state_ex="Texas"){
  # Open files with epi_prob data for all R0 run and put in one data frame
  full_df=data.frame(R0=double(), cases_detected=double(), epi_prob=double(), scam_epi_prob=double())
  for(val in 1:length(r_not_vect)){
    temp_df = read.csv( paste0(dir_path, "/epi_prob_data_", r_not_vect[val],"_", det_prob, "_0_1e+05.csv"), 
                        header = TRUE)
    R0=rep(r_not_vect[val], length(temp_df$detected))
    temp_df = cbind(R0, temp_df)
    full_df = rbind(full_df, temp_df)
  }
  names(full_df) = c("R0", "cases_detected", "epi_prob", "scam_epi_prob")
  full_df$R0 = factor(full_df$R0)
  
  full_df=subset(full_df, cases_detected<=50)
  full_df$epi_prob = full_df$epi_prob*100 # convert probability to percent
  
  county_dates_df <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") %>%
    filter(state==state_ex) %>%
    filter(county==county_ex) %>%
    arrange(date) %>%
    slice(1:8)
  
  case1 = county_dates_df$cases[1]
  label1 =  format(as.Date(county_dates_df$date[1]), "%B %d")
  case8 = county_dates_df$cases[8]
  label8 = format(as.Date(county_dates_df$date[8]), "%B %d")
  
  # Plot cases detected by epidemic risk
  case_risk_plot=ggplot(full_df, aes(x=cases_detected, y=epi_prob, group=R0, color=R0, shape=R0))+
    geom_vline(data=county_dates_df, aes(xintercept = cases[1]), 
               color="red", alpha=0.5, show.legend = FALSE )+
    geom_vline(data=county_dates_df, aes(xintercept = cases[8]),
               color="red", alpha=0.5, show.legend = FALSE)+ 
    geom_label(aes(x = case1, y = 5 ), label=label1, show.legend = FALSE, size = 2, color="black",
               inherit.aes = F)+
    geom_label(aes(x = case8, y = 5 ), label=label8, show.legend = FALSE, size = 2, color="black",
               inherit.aes = F)+
    #guides(linetype = guide_legend(override.aes = list(alpha = 0.5), title = ""))+
    geom_line()+
    geom_point()+
    scale_colour_grey()+
    expand_limits(y = 0)+
    xlab("Cumulative reported cases")+
    ylab("Epidemic risk (%)")+
    labs(color=expression(R[e]), shape=expression(R[e]))+
    theme_bw(base_size = 8)+
    theme(panel.grid.minor = element_line(colour="white", size=0.1)) +
    scale_x_continuous(minor_breaks = seq(0 , 50, 1), breaks = seq(0, 50, 5))+
    scale_y_continuous(minor_breaks = seq(0 , 110, 1), breaks = seq(0 , 110, 10))
    
  png(file=paste0(fig_path, "/case_risk_plot.png"),
      width=4.75,height=3.25, units = "in", res=1200)
  plot(case_risk_plot)
  dev.off()
}







# # Not currently used, was a check how case increase related to county pop size
# epi_risk_pop_cor_plot = function(dir_path, fig_path, date1="2020-03-16", date2="2020-03-23"){
#   
#  # Rural counties 2019
# https://www.consumerfinance.gov/compliance/compliance-resources/mortgage-resources/rural-and-underserved-counties-list/
# rural = read_csv("raw_data/cfpb_rural-list_2019.csv") %>%
#   rename(fips = `FIPS Code`) %>%
#   mutate(rural = 1) %>%
#   select(fips, rural)
# 
# # Organize all the county case data from both dates
# county_dates_df = read_csv(paste0(dir_path, "/", date_start, "county-risk-estimates.csv")) %>%
#   rename(date1     = date, cases1    = cases, deaths1   = deaths, epi_prob1 = epi_prob) %>%
#   left_join(read_csv(paste0(dir_path, "/", date_end, "county-risk-estimates.csv") ), 
#             by=c("fips", "state", "population", "county")) %>%
#   rename(date2     = date, cases2    = cases, deaths2   = deaths, epi_prob2 = epi_prob) %>%
#   mutate(diff_cases = cases2 - cases1) %>%
#   left_join(rural, by="fips") %>%
#   mutate(rural = ifelse(is.na(rural), 0, 1 ),
#          date1 = date_start,
#          date2 = date_end) # ensure dates are all correct character, 
# 
# # Select one rural and one urban county in the state choice to plot as example increase
# example_counties = county_dates_df %>%
#   filter(state==state_ex) %>%
#   filter(diff_cases < 50 & diff_cases > 0) %>%
#   filter(cases2 < 50) %>%
#   group_by(rural) %>%
#   arrange(desc(diff_cases), .by_group=T ) %>%
#   slice(1) %>%
#   ungroup()
#   
#   ggplot(sub_county_dates_df, aes(x=population, y=diff_cases))+
#     geom_point()
# 
#   names(county_dates_df)
#   
# }





