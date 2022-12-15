###########################################################
# Code to create Figures 4, 5, S2, and S9
#  and do logistic regressions with log odds increases
###########################################################

library(usmap)
library(tidyverse)
library(lubridate)
library(cowplot)
library(ggrepel)
theme_set(theme_cowplot())

epi_dat <- read_csv('processed_data/inf_period_6/validation_date_model_compare.csv')
epi_dat %>% 
  count(model_parm_set, date)

## Estimated epidemic probability March 16
epi_dat %>% 
  filter(date %in% ymd(c('2020-03-16'))) %>% 
  select(model_parm_set, fips, epi_prob, r_not) -> pred_epi_prob

## Calculate the increase vals and predictions
epi_dat %>% 
  filter(date %in% ymd(c('2020-03-23', '2020-03-16'))) %>% 
  select(model_parm_set, fips, county, state, population, r_not, date, cases) %>% 
  spread(date, cases) %>% 
  mutate(init_cases = `2020-03-16`,
         case_increase = `2020-03-23` - `2020-03-16`) %>%
  mutate(is_case_increase = ifelse(case_increase>0, 1, 0),
         is_case_increase2 = ifelse(case_increase>=2, 1, 0),
         is_case_increase5 = ifelse(case_increase>=5, 1, 0)) %>% 
  left_join(pred_epi_prob, by = c('fips', 'model_parm_set', 'r_not')) -> epi_risk_retro

just_retro <- epi_risk_retro %>% 
  filter(model_parm_set == 'retrospective')
just_original <- epi_risk_retro %>% 
  filter(model_parm_set == 'original')

## Create figure panel A
just_retro %>% 
  mutate(init_cases = ifelse(init_cases > 10, 10, init_cases)) %>% 
  group_by(init_cases) %>% 
  summarize(min_prob = min(epi_prob),
            max_prob = max(epi_prob),
            actual_prob = sum(is_case_increase)/n(),
            actual_prob2 = sum(is_case_increase2)/n(),
            actual_prob5 = sum(is_case_increase5)/n(),
  ) -> panela_df 

panela_df %>% 
  gather(key, value, actual_prob:actual_prob5) %>% 
  mutate(key = case_when(key == 'actual_prob' ~ '\u2265 1 case', # \u2265 is the >= symbol
                         key == 'actual_prob2' ~ '\u2265 2 cases',
                         T ~ '\u2265 5 cases'
  )) %>% 
  mutate(key_label = ifelse(init_cases == min(init_cases), key, NA)) %>% # warning about NA is from key_label col
  ggplot(aes(init_cases, value, color = key)) +
  geom_ribbon(data = panela_df, aes(x = init_cases, 
                                    ymin = min_prob, ymax = max_prob), alpha = .1, inherit.aes=F, fill = 'red') +
  geom_point(size = 1.5) +  
  geom_line(lty = 2) +
  geom_label_repel(aes(label = key_label), nudge_x = 3, size = 6) +
  scale_x_continuous(breaks = 0:10, labels = c(0:9, '10+'), limits = c(0,10)) +
  background_grid(major = 'y') +
  scale_color_manual(values = c('grey', 'grey40', 'black')) +
  labs(x = 'Cumulative Reported Cases (March 16)', 
       y = 'Percent Counties with Case Increase (March 23)',
       color = 'Increase by\nat least') +
  theme(legend.position = 'none') +
  scale_y_continuous(labels = scales::label_percent(), breaks = c(0, .25, .5, .75, 1), limits = c(0,1)) -> figpanela
figpanela  


# Figure panel b ----------------------------------------------------------
# Logistic Regression and plot for retrospective
retrologit1 = glm(is_case_increase ~ epi_prob, data = just_retro, family = "binomial")
summary(retrologit1)
confint(retrologit1)
retrologit2 = glm(is_case_increase2 ~ epi_prob, data = just_retro, family = "binomial")
summary(retrologit2)
confint(retrologit2)
retrologit5 = glm(is_case_increase5 ~ epi_prob, data = just_retro, family = "binomial")
summary(retrologit5)
confint(retrologit5)

make_logistic_plot <- function(log_mod, data_pts, df_col) {
  df_col <- enquo(df_col)
  x_vals <- seq(0,1,length.out = 200)
  plotdat <- data.frame(epi_prob= x_vals)
  
  # df with predictions, lower and upper limits of CIs: 
  preddat <- predict(log_mod,
                     type = "link",
                     newdata=plotdat,
                     se.fit=TRUE) %>% 
    as.data.frame() %>% 
    mutate(epi_prob = x_vals, 
           # model object has a component called linkinv that 
           # is a function that inverts the link function of the GLM:
           lower = log_mod$family$linkinv(fit - 1.96*se.fit), 
           point.estimate = log_mod$family$linkinv(fit), 
           upper = log_mod$family$linkinv(fit + 1.96*se.fit)) 
  
  
  # plotting with ggplot: 
  preddat %>% 
    ggplot(aes(x = epi_prob, 
               y = point.estimate)) + 
    geom_ribbon(aes(ymin = lower,
                    ymax = upper), 
                alpha = 0.2, fill = 'blue') + 
    geom_line(color = "blue") + 
    geom_point(data = data_pts, aes(x = epi_prob, y = !!df_col), alpha = .1) +
    scale_y_continuous(limits = c(0,1), labels = scales::label_percent()) +
    scale_x_continuous(limits = c(0,1), labels = scales::label_percent()) +
    labs(x = 'Estimated Epidemic Risk (March 16)', y = 'Probability of Case Increase (March 23)') +
    background_grid(major = 'xy') 
  
}

retrolog_p1 <- make_logistic_plot(retrologit1, just_retro, df_col = is_case_increase)
retrolog_p2 <- make_logistic_plot(retrologit2, just_retro, df_col = is_case_increase2)
retrolog_p5 <- make_logistic_plot(retrologit5, just_retro, df_col = is_case_increase5)

retrolog_p2

# Putting it all together for figure 4 ------------------------------------

fig4 <- plot_grid(figpanela, 
                  retrolog_p2, 
                  labels = c('A', 'B'), nrow = 1, 
                  rel_widths = c(1,1), align ='hv')
save_plot('figures/epi_risk_fig4.png', fig4, base_height = 5.2, base_asp = 2.2, bg = 'white')


# Figure 5 ----------------------------------------------------------------
# comparing retrospective to original

## Switch to R0 1.5 and R0 of 3...
pred_epi_prob %>% 
  filter(model_parm_set == 'original') %>% 
  select(-model_parm_set) %>% 
  rename(rt_epi_prob = epi_prob) %>% 
  left_join(pred_epi_prob %>% 
              filter(model_parm_set == 'retrospective') %>% 
              select(fips, retro_epi_prob = epi_prob), by = 'fips') %>% 
  ggplot(aes(rt_epi_prob, retro_epi_prob, color = as.factor(r_not))) + 
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  background_grid(major = 'xy') +
  scale_color_manual(values = c('grey', 'grey40', 'black')) +
  labs(x = 'Real-time Estimated Epidemic Risk (March 16)', 
       y = 'Retrospective Estimated Epidemic Risk (March 16)',
       color = expression('Real-time\nAssumed R'[e])) +
  scale_y_continuous(labels = scales::label_percent(), breaks = c(0, .25, .5, .75, 1), limits = c(0,1)) +
  scale_x_continuous(labels = scales::label_percent(), breaks = c(0, .25, .5, .75, 1), limits = c(0,1)) -> epi_prob_comparison
epi_prob_comparison

# Figure 5
save_plot('figures/epi_risk_fig5.png', epi_prob_comparison, base_height = 5, base_asp = 1.4, bg = 'white')

## Correlation analysis
get_corr_results <-function(df){
  cor.test(x = df$rt_epi_prob, y = df$retro_epi_prob) %>% broom::tidy()
}
pred_epi_prob %>% 
  filter(model_parm_set == 'original') %>% 
  select(-model_parm_set) %>% 
  rename(rt_epi_prob = epi_prob) %>% 
  left_join(pred_epi_prob %>% 
              filter(model_parm_set == 'retrospective') %>% 
              select(fips, retro_epi_prob = epi_prob), by = 'fips') %>% 
  nest(data = c(fips, rt_epi_prob, retro_epi_prob)) %>% 
  mutate(corr_results = map(data, get_corr_results)) %>% 
  unnest(corr_results)

# Supplemental figure with retro logistic regression ----------------------
plot_grid(retrolog_p1, retrolog_p5, labels = 'AUTO') -> supp_logistic_fits_retro
# Figure S2
save_plot('figures/supp_logistic_fits_retro.png', supp_logistic_fits_retro, base_height = 5, base_asp = 2, bg = 'white')

# Supplemental figure with original logistic regression ----------------------
just_original %>% 
  select(fips, r_not,  epi_prob, is_case_increase:is_case_increase5) %>% 
  gather(key, value, is_case_increase:is_case_increase5) -> all_original_dat

get_log_fit <- function(increase_level, data) {
  glm(value ~ epi_prob, data = data, family = "binomial")  -> mod
  
  x_vals <- seq(0,1,length.out = 200)
  plotdat <- data.frame(epi_prob= x_vals)
  
  # df with predictions, lower and upper limits of CIs: 
  preddat <- predict(mod,
                     type = "link",
                     newdata=plotdat,
                     se.fit=TRUE) %>% 
    as.data.frame() %>% 
    mutate(epi_prob = x_vals, 
           # model object mod1 has a component called linkinv that 
           # is a function that inverts the link function of the GLM:
           lower = mod$family$linkinv(fit - 1.96*se.fit), 
           point.estimate = mod$family$linkinv(fit), 
           upper = mod$family$linkinv(fit + 1.96*se.fit)) 
}

all_original_dat %>% 
  nest(data = c(fips, epi_prob, value)) %>% 
  mutate(log_preds = map2(key, data, get_log_fit)) %>% 
  select(-data) %>% 
  unnest(log_preds) %>% 
  mutate(key = case_when(key == 'is_case_increase' ~ '\u2265 1 case',
                         key == 'is_case_increase2' ~ '\u2265 2 cases',
                         key == 'is_case_increase5' ~ '\u2265 5 cases')) %>% 
  ggplot(aes(epi_prob, point.estimate)) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = 'blue', alpha = .2) +
  geom_line(color = 'blue') + 
  facet_grid(key~r_not) +
  geom_point(data = all_original_dat %>%   
               mutate(key = case_when(key == 'is_case_increase' ~ '\u2265 1 case',
                                      key == 'is_case_increase2' ~ '\u2265 2 cases',
                                      key == 'is_case_increase5' ~ '\u2265 5 cases')), aes(x=epi_prob, y = value), alpha = .2) +
  theme(axis.line = element_blank()) +
  geom_hline(aes(yintercept=-Inf)) + 
  geom_vline(aes(xintercept=-Inf)) +
  scale_y_continuous(limits = c(0,1), labels = scales::label_percent()) +
  scale_x_continuous(limits = c(0,1), labels = scales::label_percent()) +
  labs(x = 'Estimated Epidemic Risk (March 16)', y = 'Probability of Case Increase (March 23)') +
  background_grid(major = 'xy') -> original_log_fits

# Figure S9
save_plot('figures/original_log_fits.png', original_log_fits, base_height = 8, base_asp = 1.3, bg = 'white')




# Getting the logistic regression results for the original estimates --------
originallogit1 = glm(is_case_increase ~ epi_prob, data = just_original %>% filter(r_not == 3.0), family = "binomial")
summary(originallogit1)
confint(originallogit1)
originallogit2 = glm(is_case_increase2 ~ epi_prob, data = just_original %>% filter(r_not == 3.0), family = "binomial")
summary(originallogit2)
confint(originallogit2)
originallogit5 = glm(is_case_increase5 ~ epi_prob, data = just_original %>% filter(r_not == 3.0), family = "binomial")
summary(originallogit5)
confint(originallogit5)

## Comparing with the retrospective estimates
bbmle::AICtab(retrologit2, originallogit2, weights = TRUE, sort = FALSE)