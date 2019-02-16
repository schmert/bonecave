rm( list=ls() )
graphics.off()
windows(record=TRUE)

library(read.dbc)
library(tidyverse)

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

#---------------------------------------
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


#--------------------------------------
# assemble death summaries, looping
# over years and states
#--------------------------------------

D = data.frame()  # summary

for (this_year in 1997) {
  for (i in seq(state)) {

    this_state  = state[i]
    this_region = region[i]
    print(this_state)
    fname = paste0('./SIM-raw-data/DO',this_state,this_year,'.dbc')
    tmp   = read.dbc(fname) %>%
               filter(TIPOBITO == 2,
                      SEXO %in% 1:2) %>%  # non-fetal death
               mutate(state  = this_state,
                      region = this_region,
                      year   = this_year,
                      month  = as.numeric(substr(DTOBITO,3,4)),
                      dom    = as.numeric(substr(DTOBITO,1,2)),
                      doy    = day_number(DTOBITO),
                      dow    = day_of_week(DTOBITO)
                      ) %>%
               select(state:dow,
                      date=DTOBITO,
                      agecode = IDADE,
                      sex=SEXO,
                      res_municode = CODMUNRES,
                      occ_municode = CODMUNOCOR,
                      cause = CAUSABAS,
                      circumstance = CIRCOBITO)
  
    D = rbind(D, tmp)
  
  } # this_state
} # this_year



# BR = B %>%
#         group_by(doy) %>%
#         summarize(births = sum(births)) %>% 
#         ungroup() %>%
#         mutate(country='Brazil',
#                prop_births = births/sum(births)) %>%
#         select(country, doy, prop_births)
# 
# df = rbind( BR, 
#             US)
# 
# ggplot(data=df, aes(x=doy, y=prop_births, 
#                     color=country, group=country,
#                     fill=country)) +
#        geom_point(alpha=.30)  +
#        lims(y=c(.0015,.0035)) +
#        geom_smooth(alpha=.20, span=.60) +
#        theme_bw() +
#        labs(title='Seasonal Pattern of Births, Brazil and USA 2015',
#             x='Day of Year',y='Proportion of Annual Births') +
#        scale_x_continuous(breaks=c(1,91,182,274),
#                           minor_breaks = NULL,
#                           labels=c('1 Jan','1 Apr',
#                                    '1 Jul','1 Oct'))
# 
# ggsave(file='seasonality comparison.png', 
#        width=11, height=8.5)
# 
