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
  epi_probs <- get_epidemic_prob_by_d(trials = sims, 
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
  usmap::us_map(regions = "counties")  %>% 
    as_tibble() %>% 
    distinct(fips, full) %>% 
    inner_join(pop_data %>% 
                 mutate(fips = str_pad(as.character(fips), 5, side = "left", pad = "0")), by = "fips") %>% 
    rename(population = POPESTIMATE2019,
           county = CTYNAME) %>% 
    left_join(cty_case_data %>% 
                select(fips, date, cases, deaths), by = "fips") %>% 
    mutate(cases = ifelse(is.na(cases), 0, cases),
           epi_prob = epi_probs$prob_epidemic[cases+1]) %>% 
           #epi_prob = scam_prob_epidemic[cases+1]) %>%
    mutate(epi_prob = ifelse(is.na(epi_prob), 1, epi_prob)) %>% 
    rename(state = full)
}

plot_county_risk <- function(county_data, state = NULL){
  shp_file <- usmap::us_map(regions = "counties")
  if(!is.null(state)){
    shp_file <- shp_file %>% filter(full == state)
  }
  shp_file %>% 
    left_join(county_data, by = "fips") %>% 
    ggplot(aes(x = x, y = y)) +
    geom_polygon(aes(group = group, fill = epi_prob), color = "black", size = 0.1) +
    scale_fill_gradient(low = "gainsboro", high = "dark red", name = "Probability")+
    scale_x_continuous("", breaks = NULL) + scale_y_continuous("", breaks = NULL)+
    #labs(title = paste0("Epidemic Probability per county, Cum. Case by ", case_date, ", R0=",r0)) +
    theme(panel.background = element_rect(color = "white", fill = "white"),
          legend.title=element_text(size=8),
          legend.text=element_text(size=6))
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
    separate(col = path, into = c("junk", "junk2", "r_not", "detection_probability", "importation_rate", "num_reps"), sep = "_") %>% 
    select(-junk, -junk2)
}

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
    labs(color  = expression(R[0]), shape=expression(R[0]), linetype=expression(R[0]))+
    xlab("Case Detection Probability")+
    ylab("Percent")+
    scale_color_manual(values=c("#999999", "grey39", "#000000"))+
    theme_bw(base_size = 10)
}



make_case_risk_plot=function(r_not_vect, det_prob){
  ### Open files with epi_prob data for all R0 run and put in one data frame
  full_df=data.frame(R0=double(), cases_detected=double(), epi_prob=double(), scam_epi_prob=double())
  for(val in 1:length(r_not_vect)){
    temp_df = read.csv( paste0("processed_data/epi_prob_data_", r_not_vect[val],"_", det_prob, "_0_1e+05.csv"), header = TRUE)
    R0=rep(r_not_vect[val], length(temp_df$detected))
    temp_df = cbind(R0, temp_df)
    full_df = rbind(full_df, temp_df)
  }
  names(full_df) = c("R0", "cases_detected", "epi_prob", "scam_epi_prob")
  full_df$R0 = factor(full_df$R0)
  
  full_df=subset(full_df, cases_detected<=50)
  
  ### Plot cases detected by epidemic risk
  case_risk_plot=ggplot(full_df, aes(x=cases_detected, y=epi_prob, group=R0, color=R0, shape=R0))+ #
  #case_risk_plot=ggplot(full_df, aes(x=cases_detected, y=scam_epi_prob, group=R0, color=R0, shape=R0))+
    geom_line()+
    geom_point()+
    scale_colour_grey()+
    expand_limits(y = 0)+
    xlab("Cumulative Cases Reported")+
    ylab("Epidemic Risk")+
    labs(color="R0", shape="R0")+
    theme_bw(base_size = 8)+
    theme(panel.grid.minor = element_line(colour="white", size=0.1)) +
    scale_x_continuous(minor_breaks = seq(0 , 50, 1), breaks = seq(0, 50, 5))+
    scale_y_continuous(minor_breaks = seq(0.0 , 1.1, 0.1), breaks = seq(0, 1.1, 0.1))

  png(file="figures/case_risk_plot.png",
      width=4.25,height=3.25, units = "in", res=1200)
  plot(case_risk_plot)
  dev.off()
}







