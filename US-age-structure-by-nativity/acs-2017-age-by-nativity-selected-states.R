#####################################################
# US age by nativity counts from IPUMS ACS 2017
# "Native" if bpl in 1-120, "Foreign" otherwise
#####################################################

rm(list=ls())
graphics.off()

library(tidyverse)

# read data created from the SDA interactive 
# analysis system on ipums.org, 17 Mar 2019
df = read.csv('acs-2017-age-by-nativity-selected-states.csv') %>%
      gather(key='nativity', value='pop', -age, -state) %>%
      mutate(birth_year = 2017-age,
             state_label = factor(state,
                                  levels=c('CA','FL','KY','OH','WV'),
                                  labels=c('CALIFORNIA','FLORIDA',
                                           'KENTUCKY','OHIO','WEST VIRGINIA'))) %>%
      filter(nativity %in% c('native_born','foreign_born'),
             state %in% c('CA','FL','OH'))



ggplot(data=df, aes(x=birth_year,y=pop, fill=nativity)) +
    geom_bar(stat='identity', width=.70) +
#    guides(fill=FALSE) +
    theme_bw() +
    theme(text = element_text(face='bold',size=14)) +
    scale_y_continuous(breaks=seq(0,6e5,2e5),
                       minor_breaks=NULL,
                       labels=c(0, paste0(seq(200,600,200),',000'))) +
    scale_x_continuous(breaks=seq(1920,2018,15),
                       minor_breaks = NULL,
                       limits=c(1925,2018)) +
    labs(title='Population by Year and Place of Birth, United States 2017',
         caption='Source: ACS 2017 1-year data',
         x = 'Year of Birth',
         y = 'Population') +
    geom_text( x=1975, y=2e6, label='Native-Born', size=10) +
    geom_text( x=1975, y=3.7e6, label='Foreign-Born', size=10) +
    facet_wrap(~state_label) +
    theme(legend.position = 'bottom',
          axis.text=element_text(size=12, face='bold'))

ggsave(file='acs-2017-age-by-nativity-selected-states.png',
       width=14, height=7, units='in', dpi=400)


