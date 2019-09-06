# Mortality by day of the year, Denmark 2007-2018
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
      group_by(wk) %>%
      summarize(deaths=sum(deaths)/12)

dm    = c(31,28.25,31,30,31,30,31,31,30,31,30,31)
cumdm = cumsum(dm)  


#-------------------------
# national
ggplot(data=filter(DK,wk<53), aes(x=wk, y=deaths, group=TRUE)) +
  geom_line(color='red', lwd=0.8) +
  geom_point(color='red', size=2, shape=1) +
  labs(title='Seasonality of Deaths, Denmark 2007-2018',
       x='Week of Year',y='Avg Deaths/Week over 12 yrs',
       caption='Source: statbank.dk https://statbank.dk/statbank5a/default.asp?w=1280
') +
  scale_x_continuous(breaks=seq(1,49,4))  +
  scale_y_continuous(limits=c(900,1200),breaks=seq(900,1200,100)) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5),
        axis.title = element_text(face='bold',size=13),
        axis.text  = element_text(face='bold',size=12)) +
  geom_vline(xintercept = c(0.8,cumdm/7), lty=2) +
  annotate('text',x=cumdm/7-2, y=900, label=month.abb) +
  annotate('text', x=0,y=920,label='Approx Month (varies slightly due to leap years)',
              size=3.5, hjust=0)

#-------------------------

ggsave(file='seasonality-of-deaths-denmark-2007-2018.png',
       width=11, height=8.5, units='in', dpi=300)





