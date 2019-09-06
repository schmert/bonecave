library(tidyverse)
library(tidycensus)

# census_api_key(' ... ')

states = data.frame(
             NAME = state.name,
             abb   = state.abb
            ) %>%
         add_row( NAME='District of Columbia', abb='DC')


z = get_acs(geography='state', variables=c('B05006_024','B05006_025','B05006_151'), year=2017) %>%
      filter(is.finite(estimate)) %>%
      select(-moe) %>%
      left_join( states, by='NAME') %>%
      spread(variable,estimate) %>%
      rename(Port=B05006_024,
             Azor=B05006_025,
             Braz=B05006_151) %>%
      mutate(PT = Port+Azor) 

ggplot(data=z) + 
  aes(x=PT, y=Braz, label=abb) +
  geom_text(size=5, fontface='bold') +
  geom_abline(intercept=0,slope=1,lty=2) +
  annotate(geom='segment', x=50000, xend=50000,
           y=49000, yend=40000, lwd=2,arrow=arrow()) +
  annotate(geom='segment', x=50000, xend=50000,
           y=51000, yend=60000, lwd=2,arrow=arrow()) +
  annotate(geom='text', x=50000, y=38000, label='More Portuguese',
           size=5, fontface='bold') +
  annotate(geom='text', x=50000, y=62000, label='More Brazilians',
           size=5, fontface='bold') +
  theme_bw() +
  labs(x='Born in Portugal (incl. Açores)',
       y='Born in Brazil',
       title='Portuguese- and Brazilian-Born Residents in the US',
       caption='Source: US Census Bureau 2013-2017 American Community Survey') +
   theme(plot.title = element_text(size=20, face='bold',hjust=0.5),
         axis.title = element_text(face='bold',size=12),
         axis.text = element_text(face='bold',size=12))

ggsave(filename='Brazilian-and-Portugal-born.png', width=10, height=10,
       units='in', dpi=400)


