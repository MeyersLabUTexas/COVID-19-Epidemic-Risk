# Code to plot the median number of weeks until 1000 cases are reached depending on cases detected
# Figure S7

library(tidyverse)
library(scales)

get_cum_detections_through_time <- function(df) {
  df %>% 
    group_by(Cum_Detections) %>% 
    filter(time == min(time)) %>% 
    select(time, detected_cases = Cum_Detections)
}

r0_vect=c(1.5, 3.0)
plot_list = rep(list(NA), 2)
for(i in 1:2){
  # Load data
  load(paste0("processed_data/original_data/sim_", r0_vect[i], "_0.1_0_1e+05.rda"))
  
  # For each of the 100,000 simulations, get the total infected and detected per day
  df <- sims %>% 
    bind_rows(.id = "sim_num") %>% 
    as_tibble() %>% 
    select(sim_num, time, ninf = Cumulative_Infections, n_det = Cum_Detections)
  
  # Per number 
  df %>% 
    group_by(sim_num, n_det) %>% 
    filter(time == min(time)) %>% # Get the first day 0, 1, 2, etc. infections were detected
    group_by(n_det) %>% 
    summarize(med_inf = median(ninf), # get median and 95% CI of cumulative infections per n detected
              lb = quantile(ninf, probs = 0.025),
              ub = quantile(ninf, probs = 0.975)) %>% 
    filter(n_det <= 50) -> temp2 
  
  temp2 %>% # We detect 10% infections in model, so this checks out
    ggplot(aes(n_det, med_inf, ymin = lb, ymax = ub)) + 
    geom_point() + 
    geom_errorbar() +
    cowplot::theme_cowplot() +
    cowplot::background_grid(major = "xy", minor = "xy") +
    labs(x = "Reported cases", y = "Total infections") 
  
  ## sims reaching 1000 cases
  big_sims <- df %>% 
    filter(ninf >=1000) %>% # filter to cumulative infections greater or equal to 1000
    group_by(sim_num) %>% 
    filter(time == min(time)) %>% 
    distinct(sim_num, time)
  
  temp <- df %>% 
    group_by(sim_num, n_det) %>% 
    filter(time == min(time)) %>% # first day of each new case detection per simulation
    inner_join(big_sims %>% 
                 rename(time1k = time) , by = "sim_num") %>% 
    ungroup() %>% 
    mutate(time_diff = time1k-time) # total days it took to reach 1000 cases
  
  time_results <- temp %>% 
    group_by(n_det) %>% # per total detected cases, get the median+CI days it took to reach 1000 cases
    summarize(med_time = median(time_diff),
              lb = quantile(time_diff, probs = 0.025),
              ub = quantile(time_diff, probs = 0.975),
              med_time_wk = med_time/7,
              lb_wk = lb/7,
              ub_wk = ub/7)
  
  temp = time_results %>% 
    filter(n_det <= 50) %>%
    ggplot(aes(n_det, med_time_wk, ymin = lb_wk, ymax = ub_wk))+ 
    geom_point() + 
    geom_errorbar() +
    
    annotate('text', x=40, y=(max(time_results$ub_wk)*0.75), label=sprintf("R[e]=='%0.1f'", r0_vect[i]),
             parse=TRUE, size=3)+
    scale_x_continuous(breaks=seq(0, 50, 10))+
    cowplot::background_grid(major = "xy", minor = "xy")+
    labs(x = "Cumulative Reported Cases", y = "Weeks to 1,000 Cumulative Infections")+
    theme_bw(base_size = 8)
  
  if(i==1){
    plot_list[[i]] = ggplotGrob(temp)
  }else{
    plot_list[[i]] = ggplotGrob(temp+labs(y=""))
  } # end if
} # end for i

png(file="figures/original_data/time-to-1k-cases.png",
    width=6.75,height=3.25, units = "in", res=1200)
cowplot::plot_grid(plot_list[[1]], plot_list[[2]], align = "h", 
                   nrow=1, labels = "AUTO", label_fontface = "plain" )
dev.off()


