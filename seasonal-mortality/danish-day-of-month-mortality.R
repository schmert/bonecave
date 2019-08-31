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
      group_by(day_num) %>%
      summarize(deaths_per_day=mean(deaths))



#-------------------------
# national
ggplot(data=DK, aes(x=day_num, y=deaths_per_day, group=TRUE)) +
  geom_line(color='red', lwd=0.8) +
  geom_point(color='red', size=2, shape=1) +
  labs(title='Seasonality of Deaths, Denmark 2007-2018',
       x='Day of Month',y='Deaths/Day avg over 12 yrs',
       caption='Source: statbank.dk https://statbank.dk/statbank5a/default.asp?w=1280
') +
  scale_x_continuous(breaks=seq(1,31,5))  +
  scale_y_continuous(limits=c(0,150)) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5),
        axis.title = element_text(face='bold',size=13),
        axis.text  = element_text(face='bold',size=12))

#-------------------------

ggsave(file='day-of-month-deaths-denmark-2007-2018.png',
       width=8, height=8, units='in', dpi=300)





