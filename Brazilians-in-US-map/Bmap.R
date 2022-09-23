library(tidycensus)
library(tidyverse)
library(sf)

graphics.off()

# read "crosswalk" data with county names and codes
county_info = read_csv('US-counties.csv')

# grab American Community Survey estimates of the number of 
# Brazilian-born residents in each US county

B = get_acs(geography='county', year=2020, variables='B05006_157',
            geometry = TRUE, shift_geo = TRUE) %>% 
     mutate(state_fips = substr(GEOID,1,2),
            fips = as.numeric(GEOID)) %>% 
     left_join(county_info) %>% 
     select(GEOID, name, state, Brazilians=estimate) %>% 
     filter( !(state %in% c('AK','HI')))

# print out counts by state
B %>% 
  st_drop_geometry() %>% 
  group_by(state) %>% 
  summarize(Brazilians = sum(Brazilians), ncounties=n()) %>% 
  arrange(desc(Brazilians)) %>% 
  print(n=99)

# make supplementary little maps with national and state boundaries
US_outline = B %>% 
  group_by(TRUE) %>% 
  summarize()

state_outlines = B %>% 
  group_by(state) %>% 
  summarize()

# make a regular grid of small hexagons that covers the US
# mainland, and trim the edges to make it pretty

hexgrid = st_make_grid(B, square=FALSE, n=300) %>% 
           st_intersection(US_outline) %>% 
           st_as_sf() %>% 
           rename(geometry=x)

# set up data to calculate the estimated number of Brazilians 
# within 200 km of any hexagon's center

county_centroids = st_centroid(B) 
hex_centroids    = st_centroid(hexgrid)

nhex = nrow(hex_centroids)

dist = st_distance( hex_centroids, county_centroids) %>% 
      as.numeric() %>% 
      matrix(., nrow=nhex)

dist = dist/1000  # now in km, rather than meters

# calculate the estimated # of Brazilians within 
# a specified cutoff distance

count_Brazilians = function(ihex, cutoff_distance=100) {
  nearby_counties = which( dist[ihex,] < cutoff_distance)
  return( sum(B$Brazilians[nearby_counties]))
}

hexgrid$BR = sapply(seq(nhex), count_Brazilians, cutoff_distance=200)


# locations of consulates, translated from lat/lon coordinates
# into the same scale as the ACS geometry data (Northings and Eastings,
# maybe?)

consulates = tribble(
  ~city, ~lat, ~lon,
  'Atl', 33.75, -84.39,
  'Bos', 42.36, -71.06,
  'Chi', 41.88, -87.63,
  'Hart', 41.77, -72.67,
  'Hou', 29.76, -95.37,
  'LA' , 34.05, -118.24,
  'Mia', 25.76, -80.19,
  'NY', 40.73, -73.94,
  'SF', 37.77, -122.43,
  'Wash', 38.90, -77.00
) %>% 
  st_as_sf(coords=c('lon','lat')) %>% 
  st_set_crs('EPSG:4326') %>% 
  st_transform( crs = st_crs(B))

# make a pretty map by plotting filled hexagons and 
# omitting their boundaries. Then overlay state lines
# and consulate locations

MAP = ggplot() + 
  geom_sf(data=hexgrid, aes(fill=BR), color='transparent') +
  scale_fill_distiller(palette='YlGnBu', direction=1) +
  ggthemes::theme_map() +
  labs(title='Number of Brazilian-Born residents within 200km',
       subtitle='(dots = Brazilian consulates)',
       caption = 'US Census Bureau, ACS 2016-2020 Estimates\n@Cschmert',
       fill = '# Brazilians') +
  theme(plot.background = element_rect(color='white'),
        plot.title = element_text(size=15, hjust=0, face='bold'),
        legend.position = 'bottom') +
  geom_sf(data=state_outlines, 
            fill='transparent',  
            color='lightgrey',
            alpha=.30,lwd=0.05) +
  geom_sf(data=consulates, shape=16) +
  geom_sf_text(data=consulates, aes(label=city), hjust=0, nudge_x = 30000,
               size=2)

# save the map file in 2 formats

ggsave( filename='Bmap.pdf', plot=MAP)
ggsave( filename='Bmap.png', plot=MAP, dpi=300)