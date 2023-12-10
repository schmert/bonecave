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

text_info = filter(D, year > 1945) %>% 
             group_by(birthyear,lastname) %>% 
             slice(1)

G = ggplot(data=filter(D, year>1945)) +
  aes(x=year, y=birthyear) +
  geom_tile(fill='lightsteelblue', col='navy') +
  scale_x_continuous(breaks=seq(1950,2020,10),
                     minor_breaks = seq(1940,2022,1)) +
  scale_y_continuous(breaks=seq(1880,1960,10),
                     minor_breaks = seq(1880,1965,1)) +
  labs(x='Calendar Year', y='President\'s Birth Year',
       title='Generations: When was the US president born?',
       caption='@CSchmert, Source: https://people.math.sc.edu/Burkardt/datasets/presidents/president_timelines.csv') +
  theme_bw() +
  theme(axis.text = element_text(face='bold', size=14),
        axis.title = element_text(face='bold', size=14))

G + 
  geom_text(data=text_info, aes(x=year,y=birthyear+2.5,label=lastname),
            hjust=0.6, color='navy', fontface='bold', size=3.5) +
  geom_hline(yintercept = seq(1880,1960,20), lty='dotted')


ggsave('presidential-birthyears.png', height=8, width=8, dpi=300)
