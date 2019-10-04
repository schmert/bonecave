rm(list=ls())

graphics.off()

library(tidyverse)
library(tidycensus)
library(sf)

mykey = scan(file='my-API-key.txt', what='character')

census_api_key(mykey)

US = get_acs(year=2017, survey='acs5',
             variables=c('B05002_003'),
             summary_var = 'B05002_001',
             geography='county',
             geometry=TRUE) %>%
  mutate(pct_born_in_state = 100*estimate/summary_est,
         GEOID = as.numeric(GEOID),
         fips = floor(GEOID/1000)) %>%
  filter( !(fips %in% c(2,15,72)))


M = ggplot(data=US) +
      aes(fill=pct_born_in_state) +
      geom_sf(color=NA) + 
      scale_fill_viridis_c(option='D',direction = -1) +
      coord_sf(datum= st_crs(US)) +
      theme_void() +
      theme(panel.grid.major = element_line(colour = "transparent"),
            plot.title = element_text(hjust = 0.5, size=18, face='bold'),
            plot.caption = element_text(hjust=0.5,face='bold'),
            legend.position='bottom') +
      labs(title='% Population Born in State of Residence', 
           caption='Source: US Census Bureau, 2013-2017 ACS',
           fill='%') 
  

states = US %>%  
          group_by(fips) %>%
          summarize(pop=sum(summary_est))

M + 
  geom_sf(data=states, fill=NA, color='white', lwd=.25)

ggsave('born-in-state-of-residence.png', 
       height=8, width=11, units = 'in',dpi=300)




