###############################################
# Carl Schmertmann
# 04 September 2024
# 
# UN World Population Prospects 2024
# For each country, 
#  a. what is the year in which forecast median 
#     population reaches a maximum?
#  b. what is the maximum forecast population?
#
# If there is no "turnaround" year for a country
#  then max_year = NA and max_pop = NA
###############################################


library(wpp2024)
library(tidyverse)
library(sf)
library(maptools)
library(here)  # points to bonecave/
library(ggthemes)

rm(list=ls())
graphics.off()

data(UNlocations)
data(pop1)
data(popproj1)

national_codes = UNlocations %>% 
                   as_tibble() %>% 
                   filter(location_type == 4) %>%
                   pull(country_code)

natpop = pop1 %>% 
          filter(country_code %in% national_codes) 


natpopproj = popproj1 %>% 
  filter(country_code %in% national_codes) 


# append the projected populations 
pop = inner_join(natpop, natpopproj)

# convert to long format (each obs is a 
# country-year)

pop_long = pivot_longer(data=pop, 
                   cols = -(1:2) ,
                   names_to = 'year',
                   values_to = 'pop') %>% 
      mutate(year   = as.numeric(year)) 

# find the year in 2024+ with maximum 
# forecast population for each country

maxpop = pop_long %>% 
          filter(year > 2023) %>% 
          group_by(name,country_code) %>% 
          summarize(max_pop = max(pop),
                    max_year = year[which.max(pop)],
                    categ = case_when(
                      max_year == 2024 ~ 'Before 2024',
                      max_year %in% 2025:2049 ~ '2024-2049',
                      max_year %in% 2050:2099 ~ '2050-2099',
                      max_year == 2100        ~ 'After 2100'
                    ) %>% factor(.,levels=c('Before 2024','2024-2049','2050-2099','After 2100')))


# set up world map from {maptools}
data(wrld_simpl)

# join with forecast data
world_map = wrld_simpl %>% 
             st_as_sf() %>% 
             rename(country_code = UN, name = NAME) %>% 
             filter(!(ISO3 %in% c('ATA','ATF'))) %>% 
             left_join(maxpop, by='country_code')


# create graphic map

png(filename=here('UN-WPP','peak-population-forecast.png'),
    width=9, height=6, units='in', res=300)

  # some nice colors from canva palettes
  hues = canva_palettes$'Fresh and bright'[c(4,3,1,2)]
  
  G = ggplot(data=world_map) +
    geom_sf(aes(fill=categ)) +
    scale_fill_manual(values=hues) +  
    labs(title='When will population start to decline?',
         subtitle='UN World Population Prospects Forecasts',
         caption='WPP 2024\n@CSchmert',
         fill='Year with Maximum\nForecast Population') +
    theme_map() +
    theme(plot.title=element_text(size=20,hjust=0.5),
          plot.subtitle = element_text(size=15,hjust=0.5),
          legend.text = element_text(size=12),
          legend.title = element_text(size=12))
  
  print(G)

dev.off()
