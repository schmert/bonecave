###############################################
# Carl Schmertmann
# 26 Nov 2024
# 
# UN World Population Prospects 2024
#
# For each country-period, identify the 
# largest 5-year age group. Create maps to illustrate.
###############################################


library(wpp2024)
library(tidyverse)
library(sf)
library(maptools)
library(here)  # points to bonecave/
library(ggthemes)
library(colorspace)
library(gganimate)

rm(list=ls())
graphics.off()

# set up world map from {maptools}
data(wrld_simpl)

# join with forecast data
world_map = wrld_simpl %>% 
  st_as_sf() %>% 
  rename(country_code = UN, name = NAME) %>% 
  filter(!(ISO3 %in% c('ATA','ATF'))) 

codes_on_map = world_map %>% pull(country_code)

data(UNlocations)
data(popAge5dt)
data(popprojAge5dt)

age_group_list = unique(popAge5dt$age)

history = popAge5dt %>% 
       left_join(UNlocations) %>% 
       filter(country_code %in% codes_on_map) %>% 
       group_by(country_code,name,year) %>% 
       slice_max(pop,n=1) %>% 
       select(country_code:age) %>% 
       ungroup()

future = popprojAge5dt %>% 
        left_join(UNlocations) %>% 
        filter(country_code %in% codes_on_map) %>%
        mutate(year = as.integer(year)) %>% 
        group_by(country_code,name,year) %>% 
        slice_max(pop,n=1) %>% 
        select(country_code:age) %>% 
        ungroup()

df = bind_rows(history,future) %>% 
      mutate(age = factor(age, levels=age_group_list),
             year = factor(year))


## animation ----

  M = df %>% 
       left_join(world_map %>% select(country_code,geometry)) %>% 
       st_as_sf()

  hues <- sequential_hcl(18, "Plasma") %>% 
            rev()
  
  G = ggplot() +
    geom_sf(data= M,
            aes(fill=age),
            color=NA) +
    geom_sf(data=world_map, fill=NA,
            color='white', lwd=0.2) +
    transition_manual(year) +
    labs(title='{current_frame}',
         subtitle='Largest Five-Year Age Group in Each Country',
         caption='UN World Population Prospects 2024\n@CSchmert',
         fill='Age Group') +
    scale_fill_manual(values=hues) +
    theme_map() +
    theme(plot.title=element_text(size=36,hjust=0.5),
          plot.subtitle = element_text(size=30,hjust=0.5),
          legend.text = element_text(size=24),
          legend.title = element_text(size=24),
          legend.position = 'bottom',
          plot.caption = element_text(size=24))
  
  animate(G, end_pause = 15, fps=2,
          height = 800, width = 1200) 
  
  anim_save(here('UN-WPP',
                 'largest-age-groups-animation.gif'))
  
