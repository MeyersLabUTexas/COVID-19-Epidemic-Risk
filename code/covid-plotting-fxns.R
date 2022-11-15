###############################
## Code for plotting
###############################
save_cty_data <- function(data_path, case_date = NULL){
  cty_data <- get_cty_data(data_path, case_date)
  save("cty_data", file = str_replace(string = data_path, pattern = "sim", replacement = "county-summary"))
}

get_cty_data = function(data_path, case_date = NULL){
  #require(usmap)
  #require(scam)
  
  load(data_path)
  csv_path=str_replace(string = data_path, pattern = "rda", replacement = "csv")
  epi_path=str_replace(string = csv_path, pattern = "sim", replacement = "epi_prob_data")
  
  if(file.exists(epi_path)){
    epi_probs = read_csv(epi_path)
  }else{
    epi_probs <- get_epidemic_prob_by_d(trials = sims, 
                                        prev_threshold = 50,
                                        cum_threshold = 2000, # should match epi_thresh
                                        max_detect = 210) # the max number of cases to get epi_prob for
    epi_probs$prob_epidemic[is.na(epi_probs$prob_epidemic)]=1
    
    ## Ensure epidemic probabilities are monotonic increasing by fitting scam to data
    # scam_convert=scam(formula = epi_probs$prob_epidemic ~ s(epi_probs$detected, k = 25, bs = "micv"))
    # scam_prob_epidemic = scam_convert$fitted.values
    # scam_prob_epidemic[scam_prob_epidemic > 1]=1
    # epi_case_df = data.frame(epi_probs$detected, epi_probs$prob_epidemic, scam_prob_epidemic)
    # names(epi_case_df)=c("detected", "prob_epidemic", "scam_prob_epidemic")
    
    ## write to processed_data folder to use for plotting
    write.csv(epi_probs, epi_path, row.names = FALSE)
  }
  
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
  
  # Read in county shapefile data if it doesn't exist, otherwise create rda
  if(file.exists('processed_data/us_county_geometries.rda') ){
    load('processed_data/us_county_geometries.rda')
    print("us county data loaded")
  } else{
    us_county_geo = tidycensus::get_acs(geography="county", variables=c("B01001_001"), 
                        geometry=TRUE, year=2019, shift_geo = TRUE) %>%
      separate(NAME, into = c("county", "state"), sep = ", ") %>%
      rename(fips=GEOID)
    
    save(us_county_geo, file = 'processed_data/us_county_geometries.rda')
    print("us county data saved")
  }
  
  ## Attach all county information
  shp_file =  us_county_geo %>%
    inner_join(pop_data %>%
                 mutate(fips = str_pad(as.character(fips), 5, side = "left", pad = "0")), by = "fips") %>%
    rename(population = POPESTIMATE2019) %>% # , county = CTYNAME
    select(-CTYNAME) %>%
    left_join(cty_case_data %>% 
                select(fips, date, cases, deaths), by = "fips") %>% 
    
    mutate(cases = ifelse(is.na(cases), 0, cases),
           deaths = ifelse(is.na(deaths), 0, deaths),
           epi_prob = epi_probs$prob_epidemic[cases+1]) %>% 
    #epi_prob = scam_prob_epidemic[cases+1]) %>%
    mutate(epi_prob = ifelse(is.na(epi_prob), 1, epi_prob)) %>%
    select(-moe)
  
  return(shp_file)
}

plot_county_risk <- function(county_data, state = NULL){
  
  if(!is.null(state)){
    shp_file <- shp_file %>% filter(full == state)
  }
  
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
}

get_summary_stats <- function(county_df, state_stats = "Texas"){
  county_df %>% 
    summarize(frac_us_population = sum(population[which(epi_prob>.5)])/sum(population),
              frac_us_counties = sum(epi_prob>0.5)/n(),
              frac_state_population = sum(population[which(epi_prob>.5 & state == state_stats)]) /
                sum(population[which(state == state_stats)]),
              frac_state_counties = sum(epi_prob>0.5 & state == state_stats)/sum(state == state_stats),
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
    separate(col = keep, into = c("junk3", "r_not", "gen_time", "num_reps"), sep = "_") %>% 
    select(-starts_with("junk"))
}

# Plot only works if you've varied the detection probability
# plot_county_summary_sensitivity <- function(df){
#   df %>% 
#     select(-frac_state_counties, - frac_state_population) %>% 
#     gather(key, value, frac_us_population, frac_us_counties) %>% 
#     mutate(key = ifelse(key == 'frac_us_population', "US Population", "US Counties")) %>% 
#     ggplot(aes(detection_probability, value, color = as.factor(r_not), group = r_not, shape=as.factor(r_not)))+
#     geom_line(size=1) + 
#     geom_point(size=2) +
#     scale_y_continuous(labels = scales::percent, limits = c(0,1))+
#     facet_wrap(~key) +
#     background_grid(major = 'xy')+
#     labs(color  = expression(R[e]), shape=expression(R[e), linetype=expression(R[e]))+
#     xlab("Case Detection Probability")+
#     ylab("Percent")+
#     scale_color_manual(values=c("#999999", "grey39", "#000000"))+
#     theme_bw(base_size = 10)
# }

make_case_risk_plot=function(folder_path, fig_path, r_not_vect, gen_time, sys_date){
  ### Open files with epi_prob data for all R0 run and put in one data frame
  full_df=data.frame(R0=double(), cases_detected=double(), epi_prob=double(), scam_epi_prob=double())
  for(val in 1:length(r_not_vect)){
    temp_df = read.csv( paste0(folder_path, "/epi_prob_data_", 
                               r_not_vect[val],"_", gen_time, "_1e+05_", sys_date, ".csv"), header = TRUE)
    R0=rep(r_not_vect[val], length(temp_df$detected))
    temp_df = cbind(R0, temp_df)
    full_df = rbind(full_df, temp_df)
  }
  names(full_df) = c("R0", "cases_detected", "epi_prob")
  full_df$R0 = factor(full_df$R0)
  
  full_df=subset(full_df, cases_detected<=50)
  
  ### Plot cases detected by epidemic risk
  case_risk_plot=ggplot(full_df, aes(x=cases_detected, y=epi_prob, group=R0, color=R0))+ # , shape=R0
    geom_line()+
    geom_point()+
    scale_colour_grey()+
    expand_limits(y = 0)+
    xlab("Cumulative reported cases")+
    ylab("Epidemic risk (%)")+
    labs(color=expression(R[e]))+ # , shape=expression(R[e])
    theme_bw(base_size = 8)+
    theme(panel.grid.minor = element_line(colour="white", linewidth=0.1)) +
    scale_x_continuous(minor_breaks = seq(0 , 50, 1), breaks = seq(0, 50, 5))+
    scale_y_continuous(minor_breaks = seq(0.0 , 1.1, 0.1), breaks = seq(0, 1.1, 0.1))

  png(file=paste0(fig_path, "/case_risk_plot_", gen_time, ".png"),
      width=4.25,height=3.25, units = "in", res=1200)
  plot(case_risk_plot)
  dev.off()
}



