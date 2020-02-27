rm(list=ls())

library(tidyverse)
library(showtext)

font_add_google("Fira Sans", "Fira")
showtext_auto() 

for (yr in 2015:2017) {
  
  # omit metadata on last 28 lines of csv file
  this_file = paste0('./12613-03-01-4-',yr,'.csv')
  
  valid_input_text = head( readLines(this_file), -28)  # discard metadata at bottom of table
  
  assign(paste0('D',yr), 
         read.csv2(text=valid_input_text,  skip=8, header=FALSE,
                   na.strings=c('.','-'),
                   col.names = c('year','code','place',
                                 'total',month.abb[1:12]),
                   colClasses = c('integer','character','character',
                                  rep('integer',13))))
}

D2015 = D2015 %>%
         gather(value='deaths',key='month',-year,-code,-place)

D2016 = D2016 %>%
  gather(value='deaths',key='month',-year,-code,-place)

D2017 = D2017 %>%
  gather(value='deaths',key='month',-year,-code,-place)

D = rbind(D2015, 
          D2016, 
          D2017)



states = filter(D, nchar(code)==2, 
                   code != 'DG',
                   month != 'total') %>%
          mutate(date = lubridate::dmy(paste('1',month,year)))


ggplot(data=states) +
  aes(x=date, y=deaths, color=code, group=code) +
  geom_point() +
  geom_line() +
  theme_bw() +
  theme(text=element_text(family='Fira',size=40)) +
  guides(color=FALSE) +
  facet_wrap(~place, nrow=4, scales='free') +
  scale_x_date() +
  labs(title  = 'Deaths by Month, Germany 2015-2017',
       caption= 'Source: regionalstatistik.de',
       x      = 'Month')

ggsave(file='German-deaths-by-state-and-month.png',
       width=11, height = 8, dpi=300)


