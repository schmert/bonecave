###############################################
# observed and forecast youth and old-age fractions
# from UN WPP 2019 data
###############################################

library(wpp2019)
library(tidyverse)
library(gganimate)
library(gifski)

rm(list=ls())

data(pop)
data(popproj)

yage = 20
oage = 60
xlab = paste0('Fraction ',oage,"+")
ylab = paste0('Fraction Under ',yage)

# keep the names in alphabetical order to avoid color mismatches
sel = tribble(
 ~name, ~txt, ~code, ~color,
 'Africa','Africa', 903, 'darkgreen',
 'Asia', 'Asia', 935, 'blue',
 'Europe','Europe', 908, 'red',
 'Latin America and the Caribbean', 'Lat Am/Caribb', 904, 'purple',
 'Northern America', 'N. America', 905, 'turquoise'
) 

# observed populations up through 2020 ----

FF = popF %>%
        filter(country_code %in% sel$code) %>% 
        gather(key='year', value='Fpop', -name, -age)

MM = popM %>%
        filter(country_code %in% sel$code) %>% 
        gather(key='year', value='Mpop', -name, -age)

big_obs = full_join(FF,MM) %>%
           mutate(pop = Fpop + Mpop) %>%
           transform(x = seq(0,100,5)) %>%
           group_by(name,year) %>%
           summarize(total = sum(pop), 
                     young = sum(pop[x <  yage])/total,
                     elder = sum(pop[x >= oage])/total)



# median forecasts through 2100 ----

FF = popFprojMed %>%
  filter(country_code %in% sel$code) %>% 
  gather(key='year', value='Fpop', -name, -age)

MM = popMprojMed %>%
  filter(country_code %in% sel$code) %>% 
  gather(key='year', value='Mpop', -name, -age)

big_pred = full_join(FF,MM) %>%
  mutate(pop = Fpop + Mpop) %>%
  transform(x = seq(0,100,5)) %>%
  group_by(name,year) %>%
  summarize(total = sum(pop), 
            young = sum(pop[x <  yage])/total,
            elder = sum(pop[x >= oage])/total)

@@@some problem with factors here...
big = bind_rows(big_obs,
                big_pred) %>%
      mutate(year = as.integer(year)) %>%
      left_join(sel, by='name')

A = ggplot(data=big) +
      aes(x=elder, y=young, color=name, group=name, label=txt) +
      geom_point(aes(size=total), alpha=.70) +
      scale_size_continuous(range=c(0,30)) +
      scale_color_manual(values=sel$color) +
      geom_text(nudge_y=-.04, size=5) +
      guides(size=FALSE,color=FALSE) +
      labs(x=xlab,y=ylab,
           title='xx',
           caption='Source: UN World Population Prospects 2019') +
      geom_line(size=1) +
      theme_bw() +
      transition_reveal(year)

animate(
  A,
  fps = 5,# 24,
  duration = 30,
  width    = 500,
  height   = 500,
  renderer = gifski_renderer('age-structure.gif')
)

    
