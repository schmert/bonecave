library(tidyverse)
library(tidycensus)

# census_api_key(' ... ')

states = data.frame(
             NAME = state.name,
             abb   = state.abb
            ) %>%
         add_row( NAME='District of Columbia', abb='DC')


z = get_acs(geography='state', variables='B05006_151', year=2017) %>%
      filter(is.finite(estimate)) %>%
      left_join( states, by='NAME')

corBR = 'royalblue' 

ggplot(data=z) + 
  aes(x=estimate,y=reorder(NAME,estimate), label=reorder(abb,estimate)) + 
  geom_segment(aes(x=0, xend=estimate, 
               y=reorder(NAME,estimate),yend=reorder(NAME,estimate)),
               lwd=2, color=corBR) +
  geom_text(size=4, nudge_x = 500, hjust=0, fontface='bold',
            color=corBR) +
  theme_bw() +
  scale_x_continuous(breaks=seq(0,81000,10000),
                     minor_breaks = seq(5000,75000,10000)) +
  labs(x='Number of Brazilian-Born Residents',
       title='Brazilian-Born Residents in the US',
       caption='Source: US Census Bureau 2013-2017 American Community Survey') +
   theme(plot.title = element_text(size=22, face='bold',hjust=0.5, color= corBR ),
         axis.text.y=element_blank(),
         axis.ticks.y=element_blank(),
         axis.title.y=element_blank(),
         axis.title.x = element_text(face='bold',size=12),
         axis.text.x = element_text(face='bold',size=12))

ggsave(filename='Brazilian-born.png', width=11, height=8,
       units='in', dpi=400)


