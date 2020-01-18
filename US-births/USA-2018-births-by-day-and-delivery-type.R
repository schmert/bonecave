rm(list=ls())
library(tidyverse)
library(scales)


D = read.table('Natality, 2016-2018 expanded.txt',
               header=TRUE, sep="\t") %>%
     mutate(day = factor(substr(Weekday,1,3),
                         levels=c('Sun','Mon','Tue',
                                  'Wed','Thu','Fri',
                                  'Sat'))) %>%
     group_by(day,Delivery.Method) %>%
     summarize(births=sum(Births)) %>%
     filter( Delivery.Method %in% c('Vaginal','Cesarean'))


ggplot(data=D) +
  aes(x=day, y=births,fill=Delivery.Method) + 
  geom_bar(position='dodge',
           stat='identity') +
  theme_bw() +
  theme(axis.text = element_text(face='bold',size=12),
        axis.title = element_text(face='bold',size=14)) +
  labs(title='Births by Day of the Week and Type of Delivery',
       subtitle='USA 2018',
       caption='Source: CDC Wonder',
       y='Births in 2018',
       x='Day of the Week') +
  scale_x_discrete(breaks=0:6, 
                   labels=c('Sun','Mon','Tue',
                            'Wed','Thu','Fri',
                            'Sat')) +
  scale_y_continuous(labels=comma) +
  scale_fill_manual(values=c('red','blue'))

ggsave(filename='USA-2018-births-by-day-and-delivery-type.png',
       height=8, width=8, units='in', dpi=300)


