#...................................................................
# Carl Schmertmann
# 16 Feb 2024
# 
# an example use of HMD data in R: plot probability of surviving
# from age 20 to 65 (1 -45q20) for men and women in Sweden over
# time
# ..................................................................

library(tidyverse)
library(HMDHFDplus)


# reading HMD data requires a username and password for the 
# site.  These have to be already stored in variables called
# "un" and "pw", respectively. 

# read period life tables for males and females

male_lt = readHMDweb(CNTRY='SWE', item='mltper_1x1',
                    username=un, password=pw) 

female_lt = readHMDweb(CNTRY='SWE', item='fltper_1x1',
                            username=un, password=pw) 

# calculate survival = (1 - 45q20) for each year

male_info = male_lt %>% 
            group_by(Year) %>% 
            summarize( male_surv = lx[Age==65] / lx[Age==20])

female_info = female_lt %>% 
  group_by(Year) %>% 
  summarize( female_surv = lx[Age==65] / lx[Age==20])

# combine the male and female information for each year

df = full_join(male_info, female_info, by='Year')

# plot the survival probabilities

ggplot(data=df) +
  geom_line(aes(x=Year,y=male_surv), color='royalblue', lwd=1) +
  geom_line(aes(x=Year,y=female_surv), color='firebrick', lwd=1) +
  geom_text(x=2000,y=.95, label='Female', color='firebrick') +
  geom_text(x=2000,y=.80, label='Male', color='royalblue') +
  scale_y_continuous(limits=c(0,1), breaks=seq(0,1,.20)) +
  theme_bw() +
  labs(title='Period Probability of Survival from 20 to 65, Sweden',
       y = 'P(Surv from 20 to 65)',
       caption='Source: Human Mortality Database https://mortality.org')
