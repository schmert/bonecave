library(tidyverse)
library(sf)

district_map = st_read(dsn='.', layer='33EE250GC_SIR')

ggplot(data=district_map) +
  geom_sf(color='grey',lwd=0.25) 

