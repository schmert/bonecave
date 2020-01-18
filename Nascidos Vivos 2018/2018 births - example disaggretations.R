rm( list=ls() )
graphics.off()
windows(record=TRUE)

library(read.dbc)
library(tidyverse)
library(scales)

# unzip all the files (temporarily)

unzip(zipfile='nascidos-vivos-preliminar-2018.zip')
      

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
  fname = paste0('DNP',this_state,'2018.dbc')
  tmp   = read.dbc(fname) %>%
            filter(PARTO %in% 1:2) %>%
            mutate(doy = day_number(DTNASC),
                   dow = day_of_week(DTNASC),
                   parto = factor(PARTO, levels=1:2,
                                  labels=c('Vaginal','Cesarean'))) %>%
            group_by(parto,dow,doy) %>%
            summarize(births=n()) %>%
            ungroup() %>%
            mutate(state=this_state, region=this_region)

  B = rbind(B, tmp)          
}


BR = B %>%
  group_by(parto,dow) %>%
  summarize(births = sum(births)) %>%
  mutate(`Delivery Type`= factor(parto,
                              levels=c('Vaginal','Cesarean')))


ggplot(data=BR) +
  aes(x=dow, y=births,fill=`Delivery Type`) + 
  geom_bar(position='dodge',
           stat='identity') +
  theme_bw() +
  theme(axis.text = element_text(face='bold',size=12),
        axis.title = element_text(face='bold',size=14)) +
  labs(title='Births by Day of the Week and Type of Delivery',
       subtitle='Brazil 2018',
       caption='Source: DATASUS http://www2.datasus.gov.br',
       y='Births in 2018',
       x='Day of the Week') +
  scale_x_discrete(breaks=0:6, 
                   labels=c('Sun','Mon','Tue',
                            'Wed','Thu','Fri',
                            'Sat')) +
  scale_y_continuous(labels=comma) +
  scale_fill_manual(values=c('darkgreen','gold'))
  
ggsave(filename='Brazil-2018-births-weekday-delivery-type.png',
                height=8, width=8, units='in', dpi=300)

# remove all the unzipped files (to save space)
fnames = unzip(zipfile='nascidos-vivos-preliminar-2018.zip', list=TRUE)$Name
file.remove(fnames)



 