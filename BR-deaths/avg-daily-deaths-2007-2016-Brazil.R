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

outfile = 'total-deaths-by-day-of-year.csv'

if (!file.exists(outfile)) {

  D = data.frame()  # summary
  
  years = 2007:2016
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
                group_by(state,year,month,dom) %>%
                summarize(deaths=n()) %>%
                ungroup()
      
       D = rbind(D, keep)
    
    } # this_state
  } # this_year
  
  write.csv(D, file=outfile, row.names = FALSE)

} else {
  D = read.csv(file=outfile)
}


df = D %>% 
      mutate(day = mdy(1e4*month + 1e2*dom + 20)) %>% 
      group_by(day) %>% 
      summarize(deaths = sum(deaths)/10) %>% 
      ungroup() 

month_labels = c("", paste(1,month.abb), "","")
month_labels[seq(1,15,2)] = ''

highlight_info = df %>% 
  filter(as.character(day) %in% c('2020-01-01','2020-12-25'))

text_color = 'darkgreen'

ggplot(data=df) + 
  aes(x=day,y=deaths) + 
  geom_point(color='royalblue') +
  geom_point(data=highlight_info, shape='square', color=text_color,size=3,alpha=.50) +
  theme_bw() +
  scale_x_date(date_breaks='1 months' ,
               minor_breaks = NULL,
               labels = month_labels) +
  scale_y_continuous(limits=c(2900, max(df$deaths))) +
  labs(title='Average Daily Deaths by Date, Brazil 2007-2016',
       caption='Source: Sistema de Informação sobre Mortalidade (SIM)\n@CSchmert') +
  geom_text(aes(x=ymd('2020-01-05'), y=highlight_info$deaths[1], label='New Year\'s Day'),
            hjust=0, size=3, color=text_color) +
  geom_text(aes(x=ymd('2020-12-20'), y=highlight_info$deaths[2], label='Christmas Day'),
            hjust=1, size=3, color=text_color) +
  geom_text(x=ymd('2020-07-01'), y=2950, label='Winter', color=text_color, size=4.5) +
  geom_text(x=ymd('2020-02-01'), y=2950, label='Carnaval', color=text_color, size=4.5,
            hjust=0)

ggsave(file='avg-daily-deaths-2007-2016-Brazil.png',
       width=10, height=8)



