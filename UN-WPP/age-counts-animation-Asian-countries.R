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
xlab = paste0('Millions of Elders (',oage,"+)")
ylab = paste0('Millions of Children (0-',yage-1,')')

# keep the names in alphabetical order to avoid color mismatches
sel = tribble(
  ~name, ~txt, ~code, ~color,
  'Bangladesh','BGD',50,'darkgreen',
  'China','CHN',156,'red',
  'India','IND',356,'orange',
  'Indonesia','IDN',360,'magenta',
  'Japan','JAP',392,'blue',
  'Pakistan','PAK',586,'green'
) 

# observed populations up through 2020 ----

FF = popF %>%
        filter(country_code %in% sel$code) %>% 
        gather(key='year', value='Fpop',-country_code, -name, -age)

MM = popM %>%
        filter(country_code %in% sel$code) %>% 
        gather(key='year', value='Mpop',-country_code, -name, -age)

big_obs = full_join(FF,MM) %>%
           mutate(pop = Fpop + Mpop) %>%
           transform(x = seq(0,100,5)) %>%
           group_by(name,year) %>%
           summarize(young = sum(pop[x <  yage])/1000,
                     elder = sum(pop[x >= oage])/1000)

# median forecasts through 2100 ----

FF = popFprojMed %>%
  filter(country_code %in% sel$code) %>% 
  gather(key='year', value='Fpop',-country_code, -name, -age)

MM = popMprojMed %>%
  filter(country_code %in% sel$code) %>% 
  gather(key='year', value='Mpop',-country_code, -name, -age)

big_pred = full_join(FF,MM) %>%
  mutate(pop = Fpop + Mpop) %>%
  transform(x = seq(0,100,5)) %>%
  group_by(name,year) %>%
  summarize(young = sum(pop[x <  yage])/1000,
            elder = sum(pop[x >= oage])/1000)

big = bind_rows(big_obs,
                big_pred) %>%
      mutate(year = as.integer(year)) %>%
      left_join(sel, by='name')

theme_carl <- function () { 
  theme_bw(base_size=11) %+replace% 
    theme(
      title      = element_text(size=28, face='bold', hjust =0.5),
      axis.text  = element_text(size=18, face='bold'),
      axis.title = element_text(size=18, face='bold')
    )
}

A = ggplot(data=big) +
      aes(x=elder, y=young, color=name, group=name, label=txt) +
      geom_point(size=5, alpha=.70) +
      geom_abline(slope=1, intercept=0, color='black',lty='dotted', size=1) +
      annotate(geom='text', x=c(300,100), y=c(200,200),
                label=c('More\nElders','More\nChildren'),
               color='black',size=6) +
      # scale_x_continuous(limits=c(0,45),
      #                    breaks=seq(0,40,10),
      #                    minor_breaks = NULL) +
      # scale_y_continuous(breaks=seq(10,60,10),
      #                    minor_breaks = NULL,
      #                    limits=c(10,65)) +
      #scale_size_area(max_size=30) +
      scale_color_manual(values=sel$color) +
      geom_text(nudge_x=40, size=5) +
      guides(size=FALSE,color=FALSE) +
      labs(x=xlab,y=ylab,
           title='Population Age Structure\n{frame_along}',
           caption='Source: UN WPP 2019') +
      geom_line(size=1) +
      theme_carl() +
      transition_reveal(year)

animate(
  A,
  fps      = 20,
  duration = 30,
  width    = 500,
  height   = 550,
  renderer = gifski_renderer('age-counts-Asian-countries.gif')
)

    
