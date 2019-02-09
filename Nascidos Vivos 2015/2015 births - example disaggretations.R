rm( list=ls() )
graphics.off()
windows(record=TRUE)

library(read.dbc)
library(tidyverse)

state = c("AC", "AL", "AM", "AP", "BA", 
          "CE", "DF", "ES", "GO", "MA", 
          "MG", "MS", "MT", "PA", "PB", 
          "PE", "PI", "PR", "RJ", "RN", "RO", 
          "RR", "RS", "SC", "SE", "SP", "TO")

region = c('N','NE','N','N','NE',
           'NE','CW','SE','CW','NE',
           'SE','CW','CW','N','NE',
           'NE','NE','S','SE','NE','N',
           'N','S','S','NE','SE','CW'
          )

geo = data.frame( i=seq(state), state, region)


library(read.dbc)

B = data.frame()

day_number = function(x) {
  tmp = as.Date(x, '%d%m%Y') %>%
         strftime(format='%j') %>%
         as.numeric()
  return(tmp)
}

day_of_week = function(x) {
  tmp = as.Date(x, '%d%m%Y') %>%
    strftime(format='%w')
  return(tmp)
}

for (i in seq(state)) {
  this_state  = state[i]
  this_region = region[i]
  print(this_state)
  fname = paste0('DN',this_state,'2015.dbc')
  tmp   = read.dbc(fname) %>%
            filter(PARTO %in% 1:2) %>%
            mutate(doy = day_number(DTNASC),
                   dow = day_of_week(DTNASC),
                   parto = factor(PARTO, levels=1:2,
                                  labels=c('Vaginal','Cesarian'))) %>%
            group_by(parto,dow,doy) %>%
            summarize(births=n()) %>%
            ungroup() %>%
            mutate(state=this_state, region=this_region)

  B = rbind(B, tmp)          
}

BR = B %>%
        group_by(doy) %>%
        summarize(births = sum(births))

ggplot(data=BR, aes(x=doy, y=births)) +
     geom_point() +
     geom_line() +
     lims(y=c(0,max(BR$births))) +
     geom_smooth() +
     theme_bw()

BR = B %>%
  group_by(dow) %>%
  summarize(births = sum(births))

ggplot(data=BR, aes(x=dow, y=births)) +
  geom_point() +
  geom_line() +
  lims(y=c(0,max(BR$births))) +
  geom_smooth() +
  theme_bw()

BR = B %>%
  mutate(region=factor(region, 
                       levels=c('N','NE','SE','S','CW'),
                       labels=c('North','Northeast','Southeast',
                                'South','Center-West'))) %>%
  group_by(region,parto,dow) %>%
  summarize(births = sum(births))

ggplot(data=BR,aes(x=dow, y=births, group=parto, color=parto)) + 
  geom_point(size=1) + 
#  lims(y=c(0,max(BR$births))) +
  geom_line(lwd=1) +
  theme_bw() +
  scale_color_manual(values=c('darkgreen','royalblue')) +
  facet_wrap(~region, scales='free') +
  scale_x_discrete(breaks=0:6, 
                     labels=c('Sun','Mon','Tue',
                              'Wed','Thu','Fri',
                              'Sat'))







 