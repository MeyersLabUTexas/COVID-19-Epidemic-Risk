library(tidyverse)

cty_case_data <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

#min(cty_case_data$date)

case_date=cty_case_data %>%
  group_by(state) %>%
  summarise(first_case_date = min(date))

#min(case_date$first_case_date)  # 2020-01-21
#max(case_date$first_case_date)  # 2021-09-17

mar16=cty_case_data %>%
  filter(date=="2020-03-16" & !is.na(fips))
# sum(mar16$cases)
# min(mar16$cases)
# max(mar16$cases)
# 
# apr13=cty_case_data %>%
#   filter(date=="2020-04-13" & !is.na(fips))
# sum(apr13$cases)

# Figure 4 computation
# get the proportion of counties with at least 3 cases on March 16
mar16_gr3 = mar16 %>%
  filter(cases >=3)
mar23_gr3 = cty_case_data %>%
  filter(date=="2020-03-23" & !is.na(fips) & cases >=3)
gr3=merge(mar16_gr3, mar23_gr3, all.x=TRUE, by=c("fips", "state", "county"))
gr3$diff=gr3$cases.y - gr3$cases.x
sum(gr3$diff>=1)/length(gr3$diff)*100
sum(gr3$diff>=5)/length(gr3$diff)*100
sum(gr3$diff>=10)/length(gr3$diff)*100

cty_case_data %>%
  filter(date=="2020-03-01" & !is.na(fips)) %>%
  summarise(perc_counties = n()/3142*100)

cty_case_data %>%
  filter(date=="2020-04-01" & !is.na(fips))%>%
  summarise(perc_counties = n()/3142*100)

cty_case_data %>%
  filter(date=="2020-05-01" & !is.na(fips))%>%
  summarise(perc_counties = n()/3142*100)



