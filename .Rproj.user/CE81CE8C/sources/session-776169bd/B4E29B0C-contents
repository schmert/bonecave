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
         mutate(state.name = tolower(state.name),
                trump_win = (trump > harris)) 

votes$state.name[which(votes$state=='DC')] = 'district of columbia'


G = ggplot(data=votes) +
  geom_point(y=0.35,x=0.5,aes(size=trump),color='red',fill='red',alpha=.50) +
  geom_point(y=0.65,x=0.5,aes(size=harris),color='blue',fill='red',alpha=.50) +
  coord_flip() +
  theme_bw() +
  theme(strip.text.x = element_text(face='bold'),
        title = element_text(size=14, face='bold')) +
  geofacet::facet_geo(~ state) +
  scale_size(range=c(0,12)) +
  guides(fill='none',color='none',size='none') +
  labs(title='2024 US Presidential Vote',
       subtitle='Areas = Votes. Red=Trump, Blue=Harris',
       caption='Source Data: https://tinyurl.com/2mcfa7ya\nAccessed 20 Nov 2024\n@CSchmert')

#........................................................
# tweak the plot to color-code facet panels
#........................................................

g <- get_geofacet_grob(G)

#.....................................................
# change each state's strip text color and
# panel color to red or blue
# with a very light grey background
#.....................................................

ix = grep('strip', g$layout$name)

for (i in ix) {
  
  j = grep('rect',g$grobs[[ i ]]$grobs[[1]]$childrenOrder)
  k = grep('text',g$grobs[[ i ]]$grobs[[1]]$childrenOrder)
  
  this_state = g$grobs[[ i ]]$grobs[[1]]$children[[k]]$children[[1]]$label 
  
  trump_win = votes %>% 
               filter(state == this_state) %>% 
               pull(trump_win)
  
  this_color = ifelse(trump_win,'red','blue')
  

  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill = 'grey95'

  g$grobs[[i]]$grobs[[1]]$children[[k]]$children[[1]]$gp$col = this_color
}


G2 = ggplotify::as.ggplot(g)


ggsave(plot=G2, file=here('US-election-2024','2024-vote.png'),
       height=7,width=9,units='in', dpi=300)

