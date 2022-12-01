####################################################################################
# Plot of Re/R0 close to 1 for both real-time and retrospective analysis
# Histograms show the total simulations that reached 2,000 cumulative infections

####################################################################################

# Load libararies and plotting functions
library(rtZIKVrisk)
library(tidyverse)

folder = c("alternate_params_test", "original_params_test")
param_type = c("Retrospective", "Real-time")
Re = c(0.95, 1.05)
full_df = data.frame()
epi_probs_all = data.frame()
for(i in 1:2){ # folder loop
  for(j in 1:2){ # R0 loop
    # Open sims
    if(folder[i]=="alternate_params_test"){
      data_path = paste0("processed_data/vary_R0_close1/", folder[i], "/sim_", Re[j], "_6_1e+05_2022-11-17.rda")
    }else{
      data_path = paste0("processed_data/vary_R0_close1/", folder[i], "/sim_", Re[j], "_0.1_0_1e+05.rda")
    }
    load(data_path)
    
    # re-write file path
    csv_path=str_replace(string = data_path, pattern = "rda", replacement = "csv")
    epi_path=str_replace(string = csv_path, pattern = "sim", replacement = "epi_prob_data")
        
    # Get list of sims into a tibble
    df <- sims %>% 
      bind_rows(.id = "sim_num") %>% 
      as_tibble() %>% 
      select(sim_num, time, Cumulative_Infections, New_Infection, Cum_Detections) # time is in days
    
    # Get values for only the last day of simulation
    max_inf = df %>%
      group_by(sim_num) %>%
      arrange(desc(time), .by_group=T ) %>%
      slice(1) %>%
      ungroup() %>%
      mutate(param_set = param_type[i],
             Re=Re[j])
    
    full_df = rbind(full_df, max_inf) # join to full table
    
    # Get Epidemic probability per case
    epi_probs <- get_epidemic_prob_by_d(trials = sims, 
                                        prev_threshold = 50,
                                        cum_threshold = 2000, # should match epi_thresh
                                        max_detect = 210) # the max number of cases to get epi_prob for
    epi_probs$prob_epidemic[is.na(epi_probs$prob_epidemic)]=1
    epi_probs = epi_probs %>%
      mutate(param_set = param_type[i],
             Re=Re[j])
    write.csv(epi_probs, epi_path, row.names = FALSE) # write to file

    epi_probs_all = rbind(epi_probs_all, epi_probs)
  } # end for j
} # end for i

# Plot all the sims cum inf

inf_plot = ggplot(full_df, aes(x=Cumulative_Infections))+
  geom_histogram()+
  facet_grid(Re~param_set)+
  labs(x="Cumulative Infections", y="Total Simulations")+
  theme_bw(base_size = 8)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

png(file="figures/case_risk_plot.png",
    width=4.75,height=3.25, units = "in", res=1200)
plot(inf_plot)
dev.off()

# Proportion sims that would be classified as an epidemic
full_df %>%
  group_by(param_set, Re) %>%
  summarise(total_2000 = sum((Cumulative_Infections>=2000 & New_Infection>=50)),
            perc_2000= (total_2000/n())*100 )

#   param_set        Re total_2000 perc_2000
# 1 Real-time      0.95          0     0    
# 2 Real-time      1.05        114     0.114
# 3 Retrospective  0.95          3     0.003
# 4 Retrospective  1.05        104     0.104





