library(HMDHFDplus)
library(tidyverse)
library(gganimate)
library(transformr)

# use your own HMD username and password below

FF = readHMDweb('USA', item='fltper_1x1',
                username = my_username,
                password = my_password) %>% 
       add_column(sex='female')

MM = readHMDweb('USA', item='mltper_1x1',
                username = my_username,
                password = my_password) %>% 
       add_column(sex='male')

D = rbind(MM,FF) %>% 
     select(Year,Age,mx,sex) %>% 
     pivot_wider(id_cols=Year:Age, 
                 values_from = mx,
                 names_from = sex) %>% 
     mutate(ratio = male/female)

G = ggplot( data=D) +
  aes(x=Age,y=ratio) +
  geom_line(lwd=1.5, color='orangered') +
  geom_hline(yintercept = 1:3, lwd=0.2, color='black') +
  geom_text(x=65, y=3.2, aes(label=Year), size=20, color='darkgrey') +
  scale_y_continuous(limits=c(0.9,3.6),breaks=1:3,minor_breaks = seq(0.5,3.5,.50)) +
  scale_x_continuous(breaks=seq(0,110,10), minor_breaks = seq(0,110,5)) +
  theme_bw() +
  theme(axis.text = element_text(face='bold', size=18),
        axis.title = element_text(face='bold', size=18),
        title = element_text(face='bold', size=20)) +
  labs(caption='Source: Human Mortality Database',
       title = 'Male/Female Mortality Rate\nby Age - USA') +
  transition_time(Year) +
  ease_aes('linear', interval = 0.500)

A = animate(plot=G, nframes=length(unique(D$Year)), duration=50)
        
anim_save(filename='USA-sex-ratio-mortality.gif', animation=A)

