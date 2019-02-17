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

years = 1997:2016
nyrs  = length(years)

for (this_year in years) {
  for (i in seq(state)) {

    this_state  = state[i]
    this_region = region[i]
    print(paste(this_year,this_state))
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
                      dow    = day_of_week(DTOBITO),
                      circumstance = factor(CIRCOBITO,
                                      levels=c(1:4,9),
                                      ordered=FALSE,
                                      labels=c('Accident',
                                               'Suicide','Homicide',
                                               'Other','Unknown'))
                      ) %>%
               select(state:circumstance,
                      date         = DTOBITO,
                      agecode      = IDADE,
                      sex          = SEXO,
                      res_municode = CODMUNRES,
                      occ_municode = CODMUNOCOR,
                      cause        = CAUSABAS)

    keep = tmp %>%
              filter(circumstance %in% c('Accident','Suicide','Homicide')) %>%
              mutate(Southern = (region %in% c('S','SE'))) %>%
              group_by(year,month, dom, doy, dow, Southern, circumstance) %>%
              summarize(deaths=n()) %>%
              ungroup()
    
     D = rbind(D, keep)
  
  } # this_state
} # this_year

write.csv(D, file='ASH-death-summary.csv')

# sum over states and years
df = D %>%
     filter( !(month==2 & dom==29)) %>%
     group_by(doy, dow, circumstance) %>%
     summarize( deaths=sum(deaths))

write.csv(df, file='suicide-accident-homicide.csv')     

df = df %>%
       group_by(doy, circumstance) %>%
       summarize(deaths_per_day = sum(deaths)/nyrs)

ggplot( data=df, aes(x=doy, y=deaths_per_day, color=circumstance)) +
    geom_line() +
    geom_smooth() +
    theme_bw() +
     labs(title='Registered Deaths, Brazil 1997-2016',
       caption='Source: SIM/Datasus http://www.datasus.gov.br',
       x='Day of Year',
       y='Avg Daily Deaths (all years)') +
  scale_x_continuous(breaks=c(1,91,182,274),
                     minor_breaks =c(32,60,121,152,213,244,305,335),
                     labels=c('1 Jan','1 Apr',
                              '1 Jul','1 Oct'))


ggsave(file='ASH-deaths-Brazil-1997-2016.png',
       width=11, height=8.5)



for (k in c('Suicide','Homicide','Accident')) {

  tmp = df %>%
         filter(circumstance== k)
  
  G = ggplot( data=tmp, aes(x=doy, y=deaths_per_day)) +
    geom_line(color='red') +
    geom_smooth(color='red', fill='orangered', alpha=.20) +
    theme_bw() +
    labs(title=paste('Registered Deaths by',k,' - Brazil 1997-2016'),
         caption='Source: SIM/Datasus http://www.datasus.gov.br',
         x='Day of Year',
         y='Avg Daily Deaths (all years)') +
         scale_x_continuous(breaks=c(1,91,182,274),
                             minor_breaks = c(32,60,121,152,213,244,305,335),
                             labels=c('1 Jan','1 Apr',
                                      '1 Jul','1 Oct'))

  print(G)
  
  ggsave(file=paste0(k,'-deaths-Brazil-1997-2016.png'),
         width=11, height=8.5)
  
}



