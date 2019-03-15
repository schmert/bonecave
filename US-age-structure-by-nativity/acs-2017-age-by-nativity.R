#####################################################
# US age by nativity counts from IPUMS ACS 2017
# "Native" if bpl in 1-120, "Foreign" otherwise
#####################################################

rm(list=ls())
graphics.off()

library(tidyverse)

# read data created from the SDA interactive 
# analysis system on ipums.org, 15 Mar 2019
df = read.csv('acs-2017-age-by-nativity.csv') %>%
      gather(key='nativity', value='pop', -age) %>%
      mutate(birth_year = 2017-age) %>%
      filter(nativity %in% c('native_born','foreign_born'))



ggplot(data=df, aes(x=birth_year,y=pop, fill=nativity)) +
    geom_bar(stat='identity') +
    guides(fill=FALSE) +
    theme_bw() +
    theme(text = element_text(face='bold',size=14)) +
    scale_y_continuous(breaks=seq(0,5e6,1e6),
                       minor_breaks=NULL,
                       labels=c(0, paste0(1:5,' Mil'))) +
    scale_x_continuous(breaks=seq(1920,2018,5),
                       minor_breaks = NULL,
                       limits=c(1925,2018)) +
    labs(title='Population by Year and Place of Birth, United States 2017',
         caption='Source: ACS 2017 1-year data',
         x = 'Year of Birth',
         y = 'Population') +
    geom_text( x=1975, y=2e6, label='Native-Born', size=10) +
    geom_text( x=1975, y=3.7e6, label='Foreign-Born', size=10) 

ggsave(file='acs-2017-age-by-nativity.png',
       width=11, height=5, units='in')


