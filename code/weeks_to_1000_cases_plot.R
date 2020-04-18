# Code to plot the number of weeks until 1000 cases are reached depending on cases detected

library(tidyverse)
library(scales)
load("processed_data/sim_1.5_0.1_0_1e+05.rda")
get_cum_detections_through_time <- function(df) {
  df %>% 
    group_by(Cum_Detections) %>% 
    filter(time == min(time)) %>% 
    select(time, detected_cases = Cum_Detections)
}
df <- sims %>% 
  bind_rows(.id = "sim_num") %>% 
  as_tibble() %>% 
  select(sim_num, time, ninf = Cumulative_Infections, n_det = Cum_Detections)
df %>% 
  group_by(sim_num, n_det) %>% 
  filter(time == min(time)) %>% 
  group_by(n_det) %>% 
  summarize(avg_inf = mean(ninf),
            med_inf = median(ninf),
            lb = quantile(ninf, probs = 0.025),
            ub = quantile(ninf, probs = 0.975)) %>% 
  filter(n_det <= 50) -> temp2 
temp2 %>% 
  ggplot(aes(n_det, med_inf, ymin = lb, ymax = ub)) + 
  geom_point() + 
  geom_errorbar() +
  cowplot::theme_cowplot() +
  cowplot::background_grid(major = "xy", minor = "xy") +
  labs(x = "Reported cases", y = "Total infections") 
## sims reaching 1000 cases
big_sims <- df %>% 
  filter(ninf >=1000) %>% 
  group_by(sim_num) %>% 
  filter(time == min(time)) %>% 
  distinct(sim_num, time)
temp <- df %>% 
  group_by(sim_num, n_det) %>% 
  filter(time == min(time)) %>% 
  inner_join(big_sims %>% 
               rename(time1k = time) , by = "sim_num") %>% 
  ungroup() %>% 
  mutate(time_diff = time1k-time)
time_results <- temp %>% 
  group_by(n_det) %>% 
  summarize(avg_time = mean(time_diff),
            med_time = median(time_diff),
            lb = quantile(time_diff, probs = 0.025),
            ub = quantile(time_diff, probs = 0.975))
time_results %>% 
  filter(n_det <= 50) %>% 
  ggplot(aes(n_det, med_time/7, ymin = lb/7, ymax = ub/7)) + 
  geom_point() + 
  geom_errorbar() +
  scale_x_continuous(breaks=seq(0,50, 10), labels = c("0", "10th", "20th", "30th", "40th", "50th"))+
  cowplot::background_grid(major = "xy", minor = "xy") +
  labs(x = "Reported Case", y = "Time to 1,000 Cumulative Cases (Weeks)")+
  theme_bw(base_size = 8) -> p1
cowplot::save_plot("figures/time-to-1k-cases.png", plot = p1, base_height = 4.1, base_aspect_ratio = 1.1)
time_results %>% write_csv("processed_data/time_results.csv")