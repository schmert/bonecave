###############################################
# Carl Schmertmann
# 04 Dec 2024
# 
# UN World Population Prospects 2024
#
# For each country-period, identify the 
# largest 10-year age group. Create maps to illustrate.
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

# set up world map from {maptools} ----
# this map does not separate Sudan and South Sudan
data(wrld_simpl)

world_map = wrld_simpl %>% 
  st_as_sf() %>% 
  rename(country_code = UN, name = NAME) %>% 
  filter(!(ISO3 %in% c('ATA','ATF'))) 

codes_on_map = world_map %>% pull(country_code)

# manually add the two codes for Sudan and South Sudan
codes_on_map = c(codes_on_map, 728, 729) 

# retrieve UN WPP data for 5-year age groups ----
data(UNlocations)
data(popAge5dt)
data(popprojAge5dt)

age_group_list = unique(popAge5dt$age)

# aggregate into 10-year groups

age_info = tibble(age=age_group_list,
                  L5= seq(0,100,5),
                  L10 = 10 * L5 %/% 10) %>% 
             mutate(group10 = map2_chr(L10,L10+9,function(x,y) paste(x,y,sep='-')))


history = popAge5dt %>% 
       left_join(UNlocations) %>% 
       left_join(age_info) %>% 
       filter(country_code %in% codes_on_map) %>% 
       mutate(old_Sudan    = country_code %in% c(728,729),
              name         = if_else(old_Sudan, 'Sudan', name),
              country_code = if_else(old_Sudan, 736, country_code)) %>% 
       summarize(.by=c(country_code,name,year,group10), pop=sum(pop)) %>% 
       group_by(country_code,name,year) %>% 
       slice_max(pop,n=1) %>% 
       rename(age=group10) %>% 
       select(country_code:age) %>% 
       ungroup()

future = popprojAge5dt %>% 
        left_join(UNlocations) %>%
        left_join(age_info) %>% 
        filter(country_code %in% codes_on_map) %>%
        mutate(old_Sudan    = country_code %in% c(728,729),
               name         = if_else(old_Sudan, 'Sudan', name),
               country_code = if_else(old_Sudan, 736, country_code)) %>% 
        mutate(year = as.integer(year)) %>% 
        summarize(.by=c(country_code,name,year,group10), pop=sum(pop)) %>% 
        group_by(country_code,name,year) %>% 
        slice_max(pop,n=1) %>% 
        rename(age=group10) %>% 
        select(country_code:age) %>% 
        ungroup()

  

df = bind_rows(history,future) %>% 
      mutate(age = factor(age, levels=unique(age_info$group10)),
             year = as.numeric(year))

## animation ----

  M = df %>% 
       left_join(world_map %>% select(country_code,geometry)) %>% 
       st_as_sf()

  ncolors = df$age %>% unique() %>% length()
  
  hues <- sequential_hcl(ncolors, "Plasma") %>% 
            rev()
  
  G = ggplot() +
    geom_sf(data= M,
            aes(fill=age),
            color=NA) +
    geom_sf(data=world_map, fill=NA,
            color='white', lwd=0.2) +
   transition_manual(year) +
   labs(title='{current_frame}',

         subtitle='Largest Ten-Year Age Group',
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
  
