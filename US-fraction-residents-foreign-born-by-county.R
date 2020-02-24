rm(list=ls())

graphics.off()

library(tidyverse)
library(tidycensus)
library(sf)
library(showtext)

font_add_google("Fira Sans", "Fira")
showtext_auto() 

## fonts and theme 
theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Fira", color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "ivory", color = NA), 
      panel.background = element_rect(fill = "ivory", color = NA), 
      legend.background = element_rect(fill = "ivory", color = NA),
      panel.border = element_blank(),
      plot.title = element_text(size=80, face='bold',hjust=0.5),
      plot.caption = element_text(size=40, face='bold',hjust=1),
      legend.text = element_text(size=40, face='bold'),
      legend.title = element_text(size=40, face='bold'),
      ...
    )
}

mykey = scan(file='my-API-key.txt', what='character')

census_api_key(mykey)

US = get_acs(year=2018, survey='acs5',
             variables=c('B05002_013'),
             summary_var = 'B05002_001',
             geography='county',
             geometry=TRUE) %>%
  mutate(FBpct = 100*estimate/summary_est,
         GEOID = as.numeric(GEOID),
         fips = floor(GEOID/1000)) %>%
  filter( !(fips %in% c(2,15,72)))


M = ggplot(data=US) +
      aes(fill=FBpct) +
      geom_sf(color=NA) + 
      scale_fill_viridis_c(option='C',direction = -1) +
      coord_sf(datum= st_crs(US)) +
      theme_map() +
      labs(title='% Population Foreign-Born', 
           caption='Source: US Census Bureau, 2014-2018 ACS',
           fill='%') 
  

states = US %>%  
          group_by(fips) %>%
          summarize(pop=sum(summary_est))

M + 
  geom_sf(data=states, fill=NA, color=grey(.60), lwd=.25)

ggsave('US-fraction-residents-foreign-born-by-county.png', 
       height=8, width=11, units = 'in',dpi=300)




