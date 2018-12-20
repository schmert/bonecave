#---------------------------------------------
# Carl Schmertmann
# created 20 Dec 18
# altered 20 Dec 18
#
# US state population change 2010-2018, animated
#---------------------------------------------
# devtools::install_github('thomasp85/gganimate')

rm(list=ls())
library(tidyverse)
library(gganimate)
library(scales)

###############
# abbreviations
tmp = data.frame( name=state.name, abb = state.abb) 
tmp = rbind(tmp,  data.frame(name=c('Puerto Rico','District of Columbia'), abb=c('PR','DC')))

D = read.csv('nst-est2018-popchg2010_2018.csv', as.is=TRUE) %>%
  filter(SUMLEV==40) %>%
  select(name    = NAME, contains('POPESTIMATE')) %>%
  left_join( tmp) %>%
  gather( key=estimate, value=pop, -name, -abb) %>%
  mutate( year= as.numeric(substr(estimate,12,15))) %>%
  select(-estimate) %>%
  group_by(name,abb) %>%
  mutate( change = pop - pop[year==2010],
          gain = factor(change>0)) %>%
  ungroup()

  

head(D)

#-----------------------------
# plot
#-----------------------------

tmp = filter( D, year %in% c(2010,2010)) 

ggplot( tmp, aes(x=pop, y=name, label=abb, color=gain)) +
  geom_point(size=2, alpha=.80, color='darkgrey') +
  geom_text(size=3, nudge_x= -1e6, fontface='bold') +
  geom_segment(aes(x=D$pop2010, xend=D$pop,  
                   y=D$name, yend=D$name), lwd=1,
               arrow=arrow(angle=25,type="open",ends="last",length = unit(0.15, "cm"))) +
  theme_bw() +
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        axis.ticks.y = element_blank()) +
  labs(x='Resident Population', 
       title='2010-2018 Population Change by State',
       caption='Source: https://www2.census.gov/programs-surveys/popest/datasets/2010-2018/national/totals/nst-est2018-popchg2010_2018.csv') +
  guides(color=FALSE)   +
  scale_color_manual(values=c('red','black')) 


# 
# G = ggplot( data=df, aes(x=Year, y=q5, color=PopName)) +
#     geom_line(lwd=2) +
#     scale_x_continuous(limits=c(1958,2020)) +
#     guides(color=FALSE) +
#     geom_text(x=df$Year+3, 
#               y=df$q5, 
#               label=df$PopName, size=5) +
#     labs(title = 'Deaths before Age 5, per 1000 newborns\nMississippi, South Carolina, Colorado, and Minnesota',
#          x='Year', y='1000 q(5)',
#          caption='Source: US Mortality Database http://usa.mortality.org') +
#     theme_bw() +
#     theme(text=element_text(face='bold', size=12)) +
#     transition_reveal(id=PopName,along=Year)
# 
# movie = animate(G, fps=5)
# 
# anim_save('q5-state.gif', animation=movie )
