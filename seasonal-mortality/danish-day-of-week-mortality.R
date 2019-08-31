# Mortality by day of the week, Denmark 2007-2018
# data from statbank.dk
# https://statbank.dk/statbank5a/default.asp?w=1280

library(tidyverse)
library(lubridate)

DK = read.csv(file='Denmark-daily-2019824173355256980895DODDAG63269149319.csv',
              na.strings = '..')
  
names(DK) = c('month','day',paste0('deaths',2007:2018))

month.names = c('January','february','March',
                'April','May','June',
                'July','August','September',
                'October','November','December')


DK = DK %>%
       filter(month != 'Total', day != 'Total') %>%
       gather(key='year', value='deaths', -month, -day) %>%
       mutate(year_num  = as.numeric(substr(year,7,11)),
              month_num = match(month,month.names),
              day_num   = match(day,1:31),
              date_num  = mdy(paste( month_num,
                                     day_num,
                                     year_num))) %>%
      filter(!is.na(date_num)) %>%
      mutate(dow = wday(date_num),
             wk  = week(date_num)) %>% 
      group_by(dow) %>%
      summarize(deaths=sum(deaths)/12)

dm    = c(31,28.25,31,30,31,30,31,31,30,31,30,31)
cumdm = cumsum(dm)  


#-------------------------
# national
ggplot(data=filter(DK), aes(x=dow, y=deaths, group=TRUE)) +
  geom_line(color='red', lwd=0.8) +
  geom_point(color='red', size=2, shape=1) +
  labs(title='Deaths by Day of the Week, Denmark 2007-2018',
       x='Day',y='Avg Deaths/Day over 12 yrs',
       caption='Source: statbank.dk https://statbank.dk/statbank5a/default.asp?w=1280
') +
  scale_x_continuous(breaks=1:7, labels=c('Sun','Mon','Tue','Wed','Thu','Fri','Sat'))  +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5),
        axis.title = element_text(face='bold',size=13),
        axis.text  = element_text(face='bold',size=12)) 

#-------------------------

ggsave(file='deaths-by-day-of-the-week-denmark-2007-2018.png',
       width=8, height=8, units='in', dpi=300)





