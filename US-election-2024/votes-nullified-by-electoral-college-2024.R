#........................................................
# map state-level popular vote for US President 2024
# .......................................................

library(tidyverse)
library(geofacet)
library(here)
library(ggplotify)

rm(list=ls())

#... VOTE DATA
state_info = tibble(state.name, state=state.abb) %>% 
              add_row(state.name='D.C.', state='DC')

votes = read_csv(file=here('US-election-2024',
                           'us-popular-vote-by-state-2024.csv'), 
                 skip=4) %>% 
         left_join(state_info) %>% 
         mutate(trump_win = (trump > harris)) 

votes$state.name[which(votes$state=='DC')] = 'District of Columbia'

#... lost votes

votes %>% 
   mutate(null_votes=other+ifelse(trump_win,harris,trump),
          null_kvotes = round(null_votes/1000))
