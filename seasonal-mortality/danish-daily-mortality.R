# Mortality by month and state, Germany 2017
# data from statbank.dk
# https://statbank.dk/statbank5a/default.asp?w=1280

library(tidyverse)

DK = read.csv(file='Denmark-daily-2019824173355256980895DODDAG63269149319.csv',
              na.strings = '..')
  
names(DK) = c('month','day',paste0('deaths',2007:2018))

month.names = c('January','february','March',
                'April','May','June',
                'July','August','September',
                'October','November','December')

month.abbs = c('Jan','Feb','Mar','Apr','May','Jun',
               'Jul','Aug','Sep','Oct','Nov','Dec')

DK = DK %>%
       filter(month != 'Total', day != 'Total') %>%
       mutate(month = factor(month, 
                             levels=month.names,
                             labels=month.abbs),
              day = factor(day,levels=1:31 )) %>%
       gather(key='year', value='deaths', -month, -day) %>%
       group_by(month,day) %>%
       summarize(deaths = sum(deaths,na.rm=TRUE)) %>%
       filter(deaths !=0) %>%
       ungroup() %>%
       mutate(doy = seq(day)) %>%
       filter( !(month=='Feb' & day==29))





#-------------------------
# national
ggplot(data=DK, aes(x=doy, y=deaths, group=TRUE)) +
  geom_line(color='red', lwd=0.8) +
  geom_point(color='red', size=2, shape=1) +
  labs(title='Deaths by Day of the Year, Denmark 2007-2018',
       x='Day of the Year',y='Total Deaths over 12 yrs',
       caption='Source: statbank.dk https://statbank.dk/statbank5a/default.asp?w=1280
') +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5),
        axis.title = element_text(face='bold',size=13),
        axis.text  = element_text(face='bold',size=12))
 
#-------------------------

ggsave(file='seasonality-of-deaths-denmark-2007-2018.png',
       width=11, height=8.5, units='in', dpi=300)





