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

for (this_year in 1997:2016) {
  for (i in seq(state)) {
    
    this_state = state[i]

    fname = paste0('./SIM-raw-data/DO',this_state,this_year,'.dbc')

    if (!file.exists(fname)) print(paste(this_year,this_state,'missing'))
    
  } # this_state
} # this_year


