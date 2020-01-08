###############################################
# observed and forecast youth and old-age fractions
# from UN WPP 2019 data
###############################################

library(wpp2019)
library(tidyverse)
library(gganimate)
library(gifski)

data(pop)
data(popproj)

yage = 20
oage = 60
xlab = paste0('Fraction ',oage,"+")
ylab = paste0('Fraction Under ',yage)

# keep the names in alphabetical order to avoid color mismatches
sel = tribble(
 ~name, ~txt, ~color,
 'Africa','Africa','darkgreen',
 'Asia', 'Asia', 'blue',
 'Europe','Europe','red',
 'Latin America and the Caribbean', 'Lat Am/Caribb', 'purple',
 'Northern America', 'N. America', 'orange'
) 
#%>%
#  mutate(name=factor(name))
               

# observed populations up through 2020 ----

FF = popF %>%
       select(-country_code) %>%
       filter(name %in% sel$name) %>% 
       gather(key='period', value='Fpop', -name, -age)

MM = popM %>%
  select(-country_code) %>%
  filter(name %in% sel$name) %>% 
  gather(key='period', value='Mpop', -name, -age)

big_obs = full_join(FF,MM) %>%
           mutate(pop = Fpop + Mpop) %>%
           transform(x = seq(0,100,5)) %>%
           group_by(name,period) %>%
           summarize(total = sum(pop), 
                     young = sum(pop[x <  yage])/total,
                     elder = sum(pop[x >= oage])/total)



# median forecasts through 2100 ----

FF = popFprojMed %>%
  select(-country_code) %>%
  filter(name %in% sel$name) %>% 
  gather(key='period', value='Fpop', -name, -age)

MM = popMprojMed %>%
  select(-country_code) %>%
  filter(name %in% sel$name) %>% 
  gather(key='period', value='Mpop', -name, -age)

big_pred = full_join(FF,MM) %>%
  mutate(pop = Fpop + Mpop) %>%
  transform(x = seq(0,100,5)) %>%
  group_by(name,period) %>%
  summarize(total = sum(pop), 
            young = sum(pop[x <  yage])/total,
            elder = sum(pop[x >= oage])/total)

big = bind_rows(big_obs,
                big_pred) %>%
      mutate(period = as.integer(period)) %>%
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
      geom_line(size=0.2) +
      theme_bw() +
      transition_reveal(period)

animate(
  A,
  fps = 5,# 24,
  duration = 30,
  width = 500,
  height = 500,
  renderer = gifski_renderer('age-structure.gif')
)

    
