rm( list=ls() )
graphics.off()
windows(record=TRUE)

library(read.dbc)
library(tidyverse)

#------------------------------------------------
# Brazil data has to be processed state by state
#------------------------------------------------

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

is_end_of_year = function(month, dom) {
  ((month==12) & (dom %in% 30:31)) |
  ((month==1 ) & (dom %in% 1:2)) 
}

# hour of death is only available from 2006 on
years = 2006:2016
nyrs  = length(years)

for (this_year in years) {
  for (i in seq(state)) {

    this_state  = state[i]
    this_region = region[i]
    print(paste(this_year,this_state))

        fname = paste0('./SIM-raw-data/DO',this_state,this_year,'.dbc')
    
        tmp   = read.dbc(fname) %>%
               mutate(state  = this_state,
                      region = this_region,
                      year   = this_year,
                      hour   = as.numeric(as.character(HORAOBITO)) %/% 100,
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
              filter(TIPOBITO == 2,
                     SEXO %in% 1:2,
                     hour %in% 0:23,
                     is_end_of_year(month,dom)
                       ) %>%  
               select(state:circumstance,
                      date         = DTOBITO,
                      agecode      = IDADE,
                      sex          = SEXO,
                      res_municode = CODMUNRES,
                      occ_municode = CODMUNOCOR,
                      cause        = CAUSABAS)
    keep = tmp %>%
              mutate(Southern = (region %in% c('S','SE'))) %>%
              group_by(year,month, dom, doy, dow, hour, Southern, circumstance) %>%
              summarize(deaths=n()) %>%
              ungroup()
    
     D = rbind(D, keep)
  
  } # this_state
} # this_year

# add a variable for hours since 00:00 on 30 Dec

calcH = function(month,dom,hour) {
  (month==12) * ( 24*(dom-30) + hour ) +
  (month==1)  * ( 48 + 24*(dom-1) + hour)
}

D = D %>% 
      mutate( H = calcH(month,dom,hour) )

write.csv(D, file='reveillon.csv')

# sum over states and years
df = D %>%
     group_by(H, circumstance) %>%
     summarize( deaths=sum(deaths)) %>%
     ungroup()

write.csv(df, file='reveillon-hours.csv')


#########################################
# first ALL deaths

tmp = df %>%
        group_by(H) %>%
        summarize(deaths = sum(deaths))

ggplot( data=tmp, aes(x=H+.5, y=deaths)) +
    geom_point(size=3, color='royalblue') +
    geom_line(color='royalblue') +
    theme_bw() +
     labs(title='Registered Deaths by Hour Before/After New Year\'s Eve',
          subtitle='Brazil 2006-2016',
       caption='Source: SIM/Datasus http://www.datasus.gov.br',
       x='Hour',
       y='Total Deaths (all years)') +
  scale_x_continuous(breaks=seq(0,72,24),
                     minor_breaks =seq(0,96,6),
                     labels=c('30 Dec','31 Dec',
                              '1 Jan','2 Jan')) +
  geom_vline(xintercept=seq(0,96,24)) +
  geom_vline(xintercept=48, lwd=2, color='orange',alpha=.50) +
  geom_vline(xintercept=12+seq(0,72,24),lty=2,col='grey') 
  

ggsave(file='reveillon-all.png',
        width=11, height=8.5)

# deaths by cause on one plot

tmp = df %>%
  filter(circumstance %in% c('Accident','Suicide','Homicide')) %>%
  group_by(H,circumstance) %>%
  summarize(deaths = sum(deaths))

ggplot( data=tmp, aes(x=H+.5, y=deaths, color=circumstance)) +
  geom_point(size=3) +
  geom_line() +
  theme_bw() +
  labs(title='Registered Deaths by Hour Before/After New Year\'s Eve',
       subtitle='Brazil 2006-2016',
       caption='Source: SIM/Datasus http://www.datasus.gov.br',
       x='Hour',
       y='Total Deaths (all years)') +
  scale_x_continuous(breaks=seq(0,72,24),
                     minor_breaks =seq(0,96,6),
                     labels=c('30 Dec','31 Dec',
                              '1 Jan','2 Jan')) +
  geom_vline(xintercept=seq(0,96,24)) +
  geom_vline(xintercept=48, lwd=2, color='orange',alpha=.50) +
  geom_vline(xintercept=12+seq(0,72,24),lty=2,col='grey') 


ggsave(file='reveillon-ASH.png',
       width=11, height=8.5)


# separate for each cause

for (k in c('Suicide','Homicide','Accident')) {

  tmp = df %>%
         filter(circumstance== k) %>%
         group_by(H) %>%
         summarize(deaths = sum(deaths))
  
         
  G=    ggplot( data=tmp, aes(x=H+.5, y=deaths)) +
           geom_point(size=3, color='royalblue') +
           geom_line(color='royalblue') +
           theme_bw() +
           labs(title=paste0(k,'s by Hour Before/After New Year\'s Eve'),
                subtitle='Brazil 2006-2016',
                caption='Source: SIM/Datasus http://www.datasus.gov.br',
                x='Hour',
                y='Total Deaths (all years)') +
           scale_x_continuous(breaks=seq(0,72,24),
                              minor_breaks =seq(0,96,6),
                              labels=c('30 Dec','31 Dec',
                                       '1 Jan','2 Jan')) +
           geom_vline(xintercept=seq(0,96,24)) +
           geom_vline(xintercept=48, lwd=2, color='orange',alpha=.50) +
           geom_vline(xintercept=12+seq(0,72,24),lty=2,col='grey') 
         
         
  print(G)

  ggsave(file=paste0('reveillon-',k,'.png'),
         width=11, height=8.5)

}



