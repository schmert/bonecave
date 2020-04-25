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
library(showtext)

font_add_google("Fira Sans", "Fira")
showtext_auto() 


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
           left_join(df) %>%
           filter(age %in% 20:29) %>%
           group_by(birthyear) %>%
           summarize(avg_unemp = mean(unemp)) %>%
           filter(is.finite(avg_unemp))


theme_carl <- function () { 
  theme_bw(base_family="Fira") %+replace% 
    theme(
      axis.text        = element_text(size=70, face='bold'),
      panel.grid.major = element_line(color=grey(.80),size=0.3),
      panel.grid.minor = element_line(color=grey(.85),size=0.1),
      strip.text       = element_text(size=50, face='bold'),
      plot.title       = element_text(size=70,hjust=0),
      plot.subtitle    = element_text(size=50,hjust=0),
      axis.title       = element_text(size=70)
    )
}

graphics.off()
png(filename='avg-unemp.png', width=11, height=8.5, 
    res=300,units='in')

ggplot(data=cohorts) +
  aes(x=birthyear, y = avg_unemp) +
  geom_line(color='royalblue', lwd=1.25) +
  scale_y_continuous(limits=c(0,8)) +
  scale_x_continuous(breaks=c(1928,1946,1965,1981)) +
  geom_vline(xintercept = c(1928,1946,1965,1981),lwd=0.5,lty='dashed') +
  labs(x='Year of Birth', y='Unemployment Rate',
       title='Avg Aggregate Unemployment Rate (All Ages)',
       subtitle='when cohort was 20-29 years old',
       caption='Source: https://fred.stlouisfed.org/series/UNRATE') +
  theme_carl() +
  geom_text(x=1937,y=0.5,size=30,label='Silent') +
  geom_text(x=1956,y=0.5,size=30,label='Boomers') +
  geom_text(x=1973,y=0.5,size=30,label='Gen X') +
  geom_text(x=1987,y=0.5,size=30,label='Millennial') 
  
dev.off()




