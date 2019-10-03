rm(list=ls())

graphics.off()

library(tidyverse)
library(tidycensus)

census_api_key('e90802bb40c08e84c7ea65ebe1f1f7cf70685bff')

FL = get_acs(year=2017, survey='acs5',
             variables=c('B05002_003'),
             summary_var = 'B05002_001',
             geography='county', state='FL',
             geometry=TRUE) %>%
  mutate(pct_born_FL = 100*estimate/summary_est) 


ggplot(data=FL) +
  aes(fill=pct_born_FL) +
  geom_sf(lwd=0.25, color='grey') + 
  scale_fill_viridis_c(option='C',direction = -1) +
  coord_sf() +
  theme_void() +
  theme(panel.grid.major = element_line(colour = "transparent")) +
  labs(title='Percent of Residents Born in Florida', 
       caption='Source: US Census Bureau 2013. 2013-2017 ACS')


ggsave('FL-born.png', height=8, width=8, units = 'in',dpi=300)



```

