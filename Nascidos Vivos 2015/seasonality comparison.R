rm( list=ls() )
graphics.off()
windows(record=TRUE)

library(read.dbc)
library(tidyverse)

#---------------------------------------
# US data (2015)
#---------------------------------------
url = 'http://vincentarelbundock.github.io/Rdatasets/csv/mosaicData/Births2015.csv'
US  = read.csv(url, header=TRUE,stringsAsFactors = FALSE) %>%
        mutate( country     = 'USA', 
                doy         = day_of_year,
                prop_births = births/sum(births)) %>%
        select(country,doy,prop_births)

#---------------------------------------
# Brazil data (2015)
# this has to be processed state by state
#---------------------------------------

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

for (i in seq(state)[1:2]) {
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
        summarize(births = sum(births)) %>% 
        ungroup() %>%
        mutate(country='Brazil',
               prop_births = births/sum(births)) %>%
        select(country, doy, prop_births)

df = rbind( BR, 
            US)

ggplot(data=df, aes(x=doy, y=prop_births, 
                    color=country, group=country,
                    fill=country)) +
       geom_point(alpha=.30)  +
       lims(y=c(.0015,.0035)) +
       geom_smooth(alpha=.20, span=.60) +
       theme_bw() +
       labs(title='Seasonal Pattern of Births, Brazil and USA 2015',
            x='Day of Year',y='Proportion of Annual Births') +
       scale_x_continuous(breaks=c(1,91,182,274),
                          minor_breaks = NULL,
                          labels=c('1 Jan','1 Apr',
                                   '1 Jul','1 Oct'))

ggsave(file='seasonality comparison.png', 
       width=11, height=8.5)

