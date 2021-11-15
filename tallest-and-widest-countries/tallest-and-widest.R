#..................................................................
# Carl Schmertmann
# 15 Nov 2021
# 
# find N-S and E-W extremes for countries
# at gadm.org (UC Davis)
# .................................................................

library(spData) 
library(sf)
library(tidyverse)

# plot world map just to verify data
ggplot(data=world) +
  geom_sf(aes(fill=continent))

# break multi-polygon countries into polygons. For the "clump"
# countries we'll paste the single polygons back together.  
# For all others we'll keep the LARGEST contiguous
# polygon only. This will
# chop off Svalbard, Alaska+Hawaii, Argentine Antarctica, etc...  

clump = c('Chile','Japan','New Zealand', 'Australia', 'Indonesia', 'Philippines')

world2 = world %>% 
          filter(type %in% c('Country','Sovereign country')) %>% 
          st_cast(., 'POLYGON') %>% 
          mutate(subarea = st_area(geom)) %>% 
          arrange(name_long, -subarea) %>% 
          group_by(name_long) %>% 
          mutate(i=seq(name_long)) %>% 
          filter( (i==1) | (name_long %in% clump)) %>% 
          summarize(glued_back_together = TRUE)


# calculate the bounding box for each (possibly chopped) country
box = sapply(world2$geom, st_bbox) %>% 
       t() %>% 
       as_tibble() %>% 
       add_column(name = world2$name_long, .before = 1) 

this_crs = st_crs(world)

# add the corners of the bounding boxes as variables
box$NW_corner = st_as_sf(box, coords=c('xmin', 'ymax'))$geometry
box$SW_corner = st_as_sf(box, coords=c('xmin', 'ymin'))$geometry
box$SE_corner = st_as_sf(box, coords=c('xmax', 'ymin'))$geometry
box$NE_corner = st_as_sf(box, coords=c('xmax', 'ymax'))$geometry

st_crs(box$NW_corner) = this_crs
st_crs(box$SW_corner) = this_crs
st_crs(box$SE_corner) = this_crs
st_crs(box$NE_corner) = this_crs


# calculate the "height" of each country in km
box_height = sapply(1:nrow(box), function(k) {
                   st_distance(box$SW_corner[k], box$NW_corner[k]) / 1000
                   })

names(box_height) = world2$name_long

# calculate the "width" of each country in km
box_width = sapply(1:nrow(box), function(k) {
  st_distance(box$SW_corner[k], box$SE_corner[k]) / 1000
})

names(box_width) = world2$name_long

# 20 tallest

tmp = box_height %>% sort() %>% rev() %>% head(20)

tibble( name=names(tmp), height_km = round(tmp))

# 20 widest

tmp = box_width %>% sort() %>% rev() %>% head(20)

tibble( name=names(tmp), width_km = round(tmp))

# Sweden, Chile, and Norway
tmp = box_height[c('Sweden','Norway','Chile','Finland')] %>% sort() %>% rev()

tibble( name=names(tmp), height_km = round(tmp))




