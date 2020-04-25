################################################
# Carl Schmertmann
# 22 Apr 2020
#
# US aggregate unemployment and recessions 
# by cohort and age
################################################
rm(list=ls())

library(tidyverse)
library(lubridate)
library(paletteer)

clean_date <- function(x, ref_year=1935){
  m <- year(x) %% 100
  year(x) <- ifelse(m > ref_year %% 100, 1900+m, 2000+m)
  x
}

UU = read_csv('UNRATE.csv') %>%
     mutate(date= clean_date(dmy(DATE)) ) %>%
     select(date,unemp=UNRATE)

RR= read.csv('USREC.csv', skip=1117,
             col.names=c('DATE','USREC')) %>%
       mutate(date= clean_date(dmy(DATE)) ) %>%
       select(date,recession=USREC)

df = UU %>% 
       group_by(yr = year(date)) %>%
       summarize(unemp = mean(unemp))

cohorts = expand.grid(birthyear = 1925:2000,
                      age       = 0:90) %>%
           mutate(yr=birthyear+age) %>%
           left_join(df)

ggplot(data=filter(cohorts,age>19, age<66)) +
  aes(x=age,y=birthyear,fill=unemp) +
  geom_tile() +
  scale_x_continuous(breaks=seq(0,90,10)) +
  scale_y_continuous(breaks=seq(1930,2000,10)) +
#  scale_fill_viridis_c(option='D',direction = -1, na.value=NA) +
  scale_fill_paletteer_c('pals::coolwarm', #'grDevices::heat.colors', #'gameofthrones::martell', #pals::coolwarm', 
                      na.value = gray(.95)) +
  geom_vline(xintercept = c(20,30),lwd=1) +
  geom_hline(yintercept = seq(1930,2000,10),lwd=0.5) +
  labs(x='Age',y='Year of Birth') +
  theme_bw() 





