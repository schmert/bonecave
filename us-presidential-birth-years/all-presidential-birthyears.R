#..............................................................
# simple grid of US presidents' years of birth (vertical axis)
# against years in office (horizontal axis)
# .............................................................

library(tidyverse)
library(lubridate)

# read timeline file, from 
# https://people.math.sc.edu/Burkardt/datasets/presidents/president_timelines.csv
 
timeline = read_csv('president_timelines.csv', skip=4) %>% 
            mutate(born  = year( as.Date(Birth, format='%d %B %Y')),
                   enter = year( as.Date(TermBegin, format='%d %B %Y')),
                   exit  = year( as.Date(TermEnd, format='%d %B %Y'))) %>% 
            select(Index, Name,LastName,born, enter, exit)

# function to make a little df corresponding to specific years for
# one president in office

years_in_office = function(Index) {
  expand.grid(year= seq(timeline$enter[Index], timeline$exit[Index],1),
              birthyear = timeline$born[Index],
              lastname  = timeline$LastName[Index])
}

D = years_in_office(1)
for (i in 2:nrow(timeline)) {
  D = rbind( D, years_in_office(i))
}

text_info = filter(D, year > 1700) %>% 
             group_by(birthyear,lastname) %>% 
             slice(1)

G = ggplot(data=filter(D, year>1700)) +
  aes(x=year, y=birthyear) +
  geom_tile(fill='lightsteelblue', col='navy') +
  scale_x_continuous(breaks=seq(1790,2020,10),
                     minor_breaks = NULL) +
  scale_y_continuous(breaks=seq(1730,1960,10),
                     minor_breaks = NULL) +
  labs(x='Calendar Year', y='President\'s Birth Year',
       title='Birth years of US Presidents',
       caption='@CSchmert, Source: https://people.math.sc.edu/Burkardt/datasets/presidents/president_timelines.csv') +
  theme_bw() +
  theme(axis.text = element_text(face='bold', size=10),
        axis.text.x = element_text(angle=45),
        axis.title = element_text(face='bold', size=12))

G + 
  geom_text(data=text_info, aes(x=year,y=birthyear+2.5,label=lastname),
            hjust=0.6, color='navy', fontface='bold', size=2) +
  geom_abline(slope=1, intercept=c(-50,-60,-70), color='red', lwd=0.3) +
  geom_text(x=1897, y=1850, label='Age 50', color='red',size=2.5) +
  geom_text(x=1910, y=1850, label='60', color='red',size=2.5) +
  geom_text(x=1920, y=1850, label='70', color='red',size=2.5) 



ggsave('all-presidential-birthyears.png', height=8, width=8, dpi=300)
