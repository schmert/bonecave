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

month_of_year = function(x) {
  tmp = as.Date(x, '%d%m%Y') %>%
    strftime(format='%m')
  return(tmp)
}


for (i in seq(state)) {
  this_state  = state[i]
  this_region = region[i]
  print(this_state)
  fname = paste0('DN',this_state,'2015.dbc')
  tmp   = read.dbc(fname) %>%
            filter(PARTO %in% 1:2, SEXO %in% 1:2)  %>%
            mutate(doy = day_number(DTNASC),
                   month = month_of_year(DTNASC),
                   dow = day_of_week(DTNASC),
                   parto = factor(PARTO, levels=1:2,
                                  labels=c('Vaginal','Cesarian'))) %>%
            group_by(month, SEXO) %>%
            summarize(births=n()) %>%
            ungroup() %>%
            mutate(state=this_state, region=this_region)

  B = rbind(B, tmp)          
}


BR = B %>%
        mutate(month=as.numeric(month)) %>%
        group_by(month) %>%
        summarize( SRAB = sum(births[SEXO==1])/sum(births[SEXO==2]),
                   births = sum(births)) %>%
        ungroup() %>%
        mutate( prop_births = births/sum(births)) %>%
        select(month, prop_births,SRAB)

ggplot(data=BR, aes(x=month, y=SRAB)) +
       geom_point(alpha=.30)  +
       geom_line() +
       theme_bw() +
       labs(title='Sex Ratio at Birth, Brazil 2015',
            x='Day of Year',y='Males/Females') +
       scale_x_continuous(breaks=c(1:12),
                          minor_breaks = NULL,
                          labels=c('Jan','Feb','Mar',
                                 'Apr','May','Jun',
                                 'Jul','Aug','Sep',
                                 'Oct','Nov','Dec'))


ggsave(file='seasonality sex ratio.png', 
       width=11, height=8.5)

