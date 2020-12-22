library(tidyverse)
library(tidycensus)
library(sf)
library(units)
library(showtext)
library(geobr)


font_add_google("Roboto", "Roboto")
showtext_auto() 

  
theme_carl <- function () { 
  theme_minimal(base_size=13, base_family="Roboto") %+replace% 
    theme(plot.title=element_text(face='bold',size=75),
          plot.caption=element_text(size=25),
          axis.text = element_blank(),
          panel.grid = element_blank())
}

D = read_municipality(code_muni = 'all', simplified = TRUE)


## lon/lat for possible origin points
GIG = c(-43.2566, -22.8053) 

# select origin
origin = st_point(GIG) %>% 
       st_sfc() %>% 
       st_set_crs('WGS84') %>% 
       st_transform(2163)



total_county_area = st_area(D)  

# US population at a given distance from 
# a selected center (distance in meters!)
P = function(this_center, this_distance) {
  buff      = st_buffer(this_center, this_distance)
  overlaps  = st_intersects(D$geometry, buff) 
  sel       = which(sapply(overlaps, length) == 1)
  asel      = st_intersection(D$geometry[sel],buff) %>%   
               st_area()
  
  prop_overlap  = asel / total_county_area[sel]
  
  pop = sum( prop_overlap * D$estimate[sel])
  return(as.numeric(pop))
}

distance_vals = seq(100,5000,100) * 1e3

pop_vals = sapply(distance_vals, 
                  function(d) P(this_center=origin, this_distance = d))

Ncircles = 10
pop_targets = sum(D$estimate) * seq(Ncircles)/Ncircles

target_distances = approx(x=pop_vals, y=distance_vals, xout=pop_targets)$y

hue = rep(c('red','white','darkblue'),50)

state_map = D %>%
             mutate(statecode = substring(GEOID,1,2)) %>% 
             group_by(statecode) %>% 
             summarize(pop=sum(estimate))


usa = D %>% 
        summarize(pop=sum(estimate))

this_map = ggplot() +
             geom_sf(data=usa, fill=NA) +
             theme_carl() +
             labs(title=paste(Ncircles,'areas of equal population'),
                  caption='Source: US Census Bureau, 2014-2018 ACS')

k=1
this_B = st_buffer(origin, target_distances[1]) %>% 
          st_intersection(usa)

this_map = this_map +
  geom_sf(data=this_B, fill=hue[k], 
          color=NA,alpha=.60)

print(this_map)

for (k in 2:length(target_distances)) {
  this_d = target_distances[k]
  last_d = target_distances[k-1]
  
  B = st_buffer(origin,this_d) %>% 
    st_difference(st_buffer(origin, last_d)) %>% 
    st_intersection(usa) 
  
  this_map = this_map +
              geom_sf(data=B, fill=hue[k], 
                      color=NA,alpha=.60)

}

xx = st_coordinates(origin)[,'X']
yy = st_coordinates(origin)[,'Y']

this_map = this_map +
            geom_sf(data=state_map, fill=NA) +
            geom_sf(data=origin,
                       shape='+',
                       color='gold',
                       size=5)

print(this_map)

ggsave(filename='population-circles.png', plot=this_map,
       dpi=300, width=9, height=5)
