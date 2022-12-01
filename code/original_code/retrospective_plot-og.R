library(usmap)
library(tidyverse)
library(ggplot2)

pop_data <- read_csv("raw_data/county_pop_2019.csv")
cty_case_data <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
epi_probs=read.csv("processed_data/original_data/epi_prob_data_1.5_0.1_0_1e+05.csv") # R0=1.5
epi_prob3=read.csv("processed_data/original_data/epi_prob_data_3_0.1_0_1e+05.csv") # R0=3.0

all_counties = usmap::us_map(regions = "counties")  %>% 
  as_tibble() %>% 
  distinct(fips, full) %>% 
  inner_join(pop_data %>% 
               mutate(fips = str_pad(as.character(fips), 5, side = "left", pad = "0")), by = "fips") %>% 
  rename(population = POPESTIMATE2019,
         county = CTYNAME) %>% 
  left_join(cty_case_data %>% 
              select(fips, date, cases, deaths), by = "fips") %>% 
  mutate(cases = ifelse(is.na(cases), 0, cases),
         epi_prob = epi_probs$prob_epidemic[cases+1], # +1 is to get correct location in vector
         epi_prob3 = epi_prob3$prob_epidemic[cases+1]) %>% # R does not index by 0
  mutate(epi_prob = ifelse(is.na(epi_prob), 1, epi_prob),
         epi_prob3 = ifelse(is.na(epi_prob3), 1, epi_prob3)) %>%
  rename(state = full)

counties_df = all_counties %>%
  distinct(fips, state, county, population) %>%
  ungroup()

start_date = subset(all_counties, date == "2020-03-16" & fips != "NA")
start_date = merge(start_date, counties_df, by = c("fips", "state", "county", "population"), all.y = TRUE)
start_date$date[is.na(start_date$date)] = "2020-03-16"
start_date$cases[is.na(start_date$cases)] = 0
start_date$deaths[is.na(start_date$deaths)] = 0
start_date$epi_prob[is.na(start_date$epi_prob)] = epi_probs$prob_epidemic[1]
start_date$epi_prob3[is.na(start_date$epi_prob3)] = epi_prob3$prob_epidemic[1]

end_date = subset(all_counties, date == "2020-03-23" & fips != "NA")
end_date = merge(end_date, counties_df, by = c("fips", "state", "county", "population"), all.y = TRUE)
end_date$date[is.na(end_date$date)] = "2020-03-23"
end_date$cases[is.na(end_date$cases)] = 0
end_date$deaths[is.na(end_date$deaths)] = 0
end_date$epi_prob[is.na(end_date$epi_prob)] = epi_probs$prob_epidemic[1]
end_date$epi_prob3[is.na(end_date$epi_prob3)] = epi_prob3$prob_epidemic[1]

both_date_df = merge(start_date, end_date, by=c("fips", "state", "county", "population"))
both_date_df$diff_cases = both_date_df$cases.y - both_date_df$cases.x
# label up to 0-10+ cases
both_date_df$case_label = both_date_df$cases.x #factor(both_date_df$cases.x)
both_date_df$case_label[both_date_df$cases.x >= 10] = 10
# at least an increase by 1 in 1 week
both_date_df$at_least_1_case[both_date_df$diff_cases>=1] = 1 
both_date_df$at_least_1_case[both_date_df$diff_cases<1] = 0
# at least an increase by 5 in 1 week
both_date_df$at_least_5_case[both_date_df$diff_cases>=5] = 1
both_date_df$at_least_5_case[both_date_df$diff_cases<5] = 0
# at least an increase by 10 in 1 week
both_date_df$at_least_10_case[both_date_df$diff_cases>=10] = 1
both_date_df$at_least_10_case[both_date_df$diff_cases<10] = 0

mean_se_df = both_date_df %>%
  group_by(case_label) %>%
  summarize(mean = mean(diff_cases),
            num_counties = sum(n()),
            stderr = sd(diff_cases)/sqrt(num_counties),
            prob_increase1 = sum(at_least_1_case)/num_counties*100, # multiply by 100 to turn prob into percent
            prob_increase5 = sum(at_least_5_case)/num_counties*100,
            prob_increase10 = sum(at_least_10_case)/num_counties*100) %>%
  ungroup()
mean_se_df$axis_label = paste0(mean_se_df$case_label, " (", mean_se_df$num_counties, ")")
mean_se_df$axis_label[11] = "10+ (65)"
mean_se_df$prob_epidemic = epi_probs$prob_epidemic[1:11]
mean_se_df$prob_epidemic3 = epi_prob3$prob_epidemic[1:11]
mean_se_df$epi_label = paste0(mean_se_df$case_label)
mean_se_df$epi_label[11] = "10+"


# Plot Probability of case increase in 1 week as function of cases
epi_plot=
  ggplot(data=mean_se_df, aes(x=case_label))+
  geom_ribbon(aes(ymin=prob_epidemic*100, ymax=prob_epidemic3*100, fill="Estimated"), alpha = 0.1)+
  geom_line(aes(y=prob_increase1, color="1", linetype= "1"), linewidth=0.3)+
  geom_point(aes(y=prob_increase1, color="1", shape="1"), size=1)+
  geom_line(aes(y=prob_increase5, color="5", linetype= "5"), linewidth=0.3)+
  geom_point(aes(y=prob_increase5, color="5", shape="5"), size=1)+
  geom_line(aes(y=prob_increase10, color="10", linetype= "10"), linewidth=0.3)+
  geom_point(aes(y=prob_increase10, color="10", shape="10"), size=1)+
  scale_x_continuous(breaks=seq(0, 10, 1), label=mean_se_df$epi_label)+
  labs(x = "Cumulative Reported Cases (March 16)",
       y = "Percent Counties with Increase in Cases (March 23)",
       color = "Increase by \nat least",
       shape = "Increase by \nat least",
       linetype = "Increase by \nat least")+
  scale_color_manual(labels=c("1 case", "5 cases", "10 cases"), #expression(paste( "Estimated, ", R[0], "=1.5", sep="")), expression(paste( "Estimated, ", R[0], "=3.0", sep="")
                     values=c("gray55", "grey35", "black"),
                     breaks=c("1", "5", "10"))+
  scale_shape_manual(labels=c("1 case", "5 cases", "10 cases"), 
                     values = c(19, 19, 19), 
                     breaks = c("1", "5", "10"))+
  scale_linetype_manual(labels=c("1 case", "5 cases", "10 cases"), 
                        values = c("dashed", "dashed", "dashed"), 
                        breaks = c("1", "5", "10"))+
  scale_fill_manual(name = '',  values=c("Estimated" = "red"))+
  theme_bw(base_size = 8)+
  theme(panel.grid.minor = element_blank(), legend.text.align=0, panel.grid.major.x = element_blank()) #legend.position = c(0.8, 0.28)

# png(file="figures/original_data/case_increase_by_risk.png",
#     width=1250, height=960, units = "px")
# plot(epi_plot)
# dev.off()

png(file="figures/original_data/case_increase_by_risk.png",
    width=4.25,height=3.25, units = "in", res=1200)
plot(epi_plot)
dev.off()

# Logistic Regression and plot
mylogit1 = glm(at_least_1_case ~ case_label, data = both_date_df, family = "binomial")
summary(mylogit1)
confint(mylogit1)
mylogit5 = glm(at_least_5_case ~ case_label, data = both_date_df, family = "binomial")
summary(mylogit5)
confint(mylogit5)
mylogit10 = glm(at_least_10_case ~ case_label, data = both_date_df, family = "binomial")
summary(mylogit10)
confint(mylogit10)




