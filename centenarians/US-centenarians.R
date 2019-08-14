library(tidyverse)

D = read.table('cMx_1x1.txt', skip=2, header=TRUE) %>%
       select(Year:Male) %>% 
       gather(-Year, -Age, key='sex', value='mx') %>%
       group_by(Year,sex) %>%
       mutate(Hx = head(cumsum(c(0,mx)),-1),
              lx = exp(-Hx))
