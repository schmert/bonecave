library(tidyverse)
library(sf)

district_map = st_read(dsn='.', layer='33SEE250GC_SIR') %>%
   filter( NM_MUNICIP == 'RIO DE JANEIRO')


## group by subdistric and count num. of districts in each
subdistrict_map = district_map %>%
              group_by(CD_GEOCODS, NM_SUBDIST) %>%
              summarize(ndistricts = n())

ggplot(data=subdistrict_map, aes(fill=NM_SUBDIST)) +
  geom_sf(color='grey',lwd=0.25) +
  guides(fill=FALSE) +
  theme_minimal()

ggsave(filename='RJ-subdistricts.png', 
       width=11, height=8, units='in', dpi=400)


