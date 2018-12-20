library(tidyverse)
library(scales)

# abbreviations
tmp = data.frame( name=state.name, abb = state.abb) 
tmp = rbind(tmp,  data.frame(name=c('Puerto Rico','District of Columbia'), abb=c('PR','DC')))

D = read.csv('nst-est2018-popchg2010_2018.csv', as.is=TRUE) %>%
      filter(SUMLEV==40,
             NAME %in% c('Florida','Puerto Rico')) %>%
      select(name    = NAME,
             contains('POPESTIMATE')) %>%
      left_join( tmp) 


## still working here...
