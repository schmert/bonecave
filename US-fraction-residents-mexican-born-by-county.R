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

vars = c('totalpop'='B01001_001', 'mexborn' = 'B05006_139')

US = get_acs(year=2018, survey='acs5',
             variables   = 'B05006_139',
             summary_var = 'B01001_001',
             geography='county',
             geometry=TRUE) %>%
  mutate(mexborn_pct = 100*estimate/summary_est,
         GEOID = as.numeric(GEOID),
         fips = floor(GEOID/1000)) %>%
  filter( !(fips %in% c(2,15,72)))


M = ggplot(data=US) +
      aes(fill=mexborn_pct) +
      geom_sf(color=NA) + 
      scale_fill_viridis_c(option='A', direction=-1) +
      coord_sf(datum= st_crs(US)) +
      theme_map() +
      labs(title='% Population Born in Mexico', 
           caption='Sources: US Census Bureau, 2014-2018 ACS. Historical Mexico shapefile from tinyurl.com/shneekq',
           fill='%') 
  

states = US %>%  
          group_by(fips) %>%
          summarize(pop=sum(summary_est))

M = M + 
     geom_sf(data=states, fill=NA, color=grey(.60), lwd=.25)

# m is Mexico 1815 boundaries, an sf object with geometry
load('Vice-Royalty-of-New-Spain.Rdata')
mex1815 = m

b = st_bbox(mex1815)
b['xmin'] = -125

mex1815 = mex1815 %>%
           st_crop(b)

st_crs(mex1815) = st_crs(US)


M + 
   geom_sf(data=mex1815, color='darkgreen', 
           fill='darkgreen', alpha=.10 ) +
   geom_text(x=-110, y=24, label='Mexico in 1815',
             color='darkgreen', size=30) +
  geom_text(x=-110, y=21, label='(Vice Royalty of New Spain)',
            color='darkgreen', size=18) +
  
ggsave('US-fraction-residents-mexican-born-by-county.png', 
       height=8, width=11, units = 'in',dpi=300)




