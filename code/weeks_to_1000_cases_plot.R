# Code to plot the number of weeks until 1000 cases are reached depending on cases detected

library(tidyverse)
library(scales)

load("processed_data/inf_period_6/sim_2.8_6_1e+05_2022-11-14.rda")
# get_cum_detections_through_time <- function(df) {
#   df %>% 
#     group_by(Cum_Detections) %>% 
#     filter(time == min(time)) %>% 
#     select(time, detected_cases = Cum_Detections)
# }

df <- sims %>% 
  bind_rows(.id = "sim_num") %>% 
  as_tibble() %>% 
  select(sim_num, time, ninf = Cumulative_Infections, n_det = Cum_Detections) # time is in days
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
            ub = quantile(time_diff, probs = 0.975),
            med_time_wk = med_time/7,
            lb_wk = lb/7,
            ub_wk = ub/7)
time_results %>% 
  filter(n_det <= 50) %>% 
  ggplot(aes(n_det, med_time_wk, ymin = lb_wk, ymax = ub_wk))+ 
  geom_point()+ 
  geom_errorbar()+
  scale_x_continuous(breaks=seq(0,50, 10))+ # , labels = c("0", "10th", "20th", "30th", "40th", "50th")
  cowplot::background_grid(major = "xy", minor = "xy")+
  labs(x = "Cumulative Reported Cases", y = "Weeks to 1,000 Cumulative Infections")+
  theme_bw(base_size = 8) -> p1

png(file="figures/inf_period_6/time-to-1k-cases.png",
    width=4.25,height=3.25, units = "in", res=1200)
plot(p1)
dev.off()

time_results %>% write_csv("processed_data/inf_period_6/time_results.csv")


