###############################################
# Carl Schmertmann
# 03 Sep 2024
# 
# UN World Population Prospects 2024
# Forecast Births and Deaths for Brazil
###############################################

library(wpp2024)
library(tidyverse)
library(here)       # points to bonecave/

rm(list=ls())
graphics.off()

data(UNlocations)
data(misc1dt)
data(miscproj1dt)

BRobs = misc1dt %>% 
      tibble() %>% 
      filter(country_code == 76)

BRproj = miscproj1dt %>%
  tibble() %>% 
  filter(country_code == 76)

BR = bind_rows(BRobs, BRproj) %>% 
      select(country_code, name, year, births, deaths ) %>%
      mutate(growth = births-deaths) %>% 
      pivot_longer(cols=c(births,deaths), 
                   names_to='event',
                   values_to='count') %>% 
      mutate(projection = (year > 2023))

# deaths exceed births starting in which year?

crossover_year = BRproj %>% 
                   filter(deaths > births) %>% 
                   pull(year) %>% 
                   min()

# create the graph (scale vital events in millions)

png(filename=here('UN-WPP','BR-births-deaths.png'),
    height=8, width=6, units='in', res=300)

  G= 
    ggplot(data=BR) +
    aes(x=year, y=count, color=event, linetype=projection) +
    geom_ribbon(data= filter(BRproj,year>= crossover_year),
                aes(x=year,ymin=births,ymax=deaths),
                fill='lightgrey',inherit.aes = FALSE) +
    
    geom_line(lwd=1.5) +
    theme_bw() +
    theme(axis.title = element_text(size=15,face='bold'),
          axis.text  = element_text(size=12, face='bold')) +
    geom_vline(xintercept = 2023.5, lty='dotted') +
    geom_hline(yintercept = 0, lty='solid',lwd=0.2) +
    scale_x_continuous(breaks=seq(1950,2100,20)) +
    scale_y_continuous(breaks=seq(0,4000,1000),
                       labels=0:4) +
    scale_color_manual(values=c('red','blue')) +
    guides(color='none',linetype='none') +
    labs(x='Ano', y='Milhões de Eventos',
         title='Fontes de Crescimento Populacional no Brasil',
         subtitle='Previsão da ONU: World Population Prospects 2024',
         caption='UN WPP\n@Cschmert') +
    geom_text(x=2000, y=  800, size=6,color='blue', label='Óbitos') +
    geom_text(x=2015, y= 3800, size=6,color='red', label='Nascimentos') +
    geom_text(x=2020, y= 1900, size=3, label='COVID',color='black') +
    geom_text(x=2080, y=2150, size=4, color='black',
              label='Encolhimento\nPopulacional\na partir de 2043', 
              fontface='bold')
  
  
  print(G)

dev.off()

