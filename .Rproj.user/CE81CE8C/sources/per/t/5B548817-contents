library(sf)
library(ggplot2)
library(gganimate)
library(tidyverse)


##Reading example data
nc <- st_read(system.file("shape/nc.shp", package="sf"))

df <- expand_grid(FIPS = nc$FIPS,
                  year = 2000:2002) %>% 
       left_join(select(nc,geometry,FIPS),
                 by=c('FIPS')) %>% 
       add_column(value = factor(sample(4,300,replace=TRUE))) %>% 
       st_as_sf()

B = ggplot() +
#     geom_sf(data=nc) +
     geom_sf(data=df, aes(fill=value))

plot_anim <- B +
  transition_manual(year) +
  labs(title="{current_frame}")

##Render animation
animate(plot_anim, end_pause = 20,
        height = 200, width = 400) # a higher res img would not upload here :(

anim_save("lixo.gif")

if (FALSE) {  
#
##Create new sf=variable of random points
A <- nc %>% 
  st_sample(size = 30) %>% 
  st_as_sf() %>% 
  dplyr::mutate(Y = st_coordinates(.)[,2])

##Create static map
B <- ggplot() +
  geom_sf(data = nc) +
  geom_sf(data = A, size = 2, col = "#3a6589")

# save static map
ggsave("static_map.png")

##Create animation with points showing up one by one
plot_anim <- B +
  transition_states(states = Y, state_length = 0, wrap = FALSE) +
  enter_recolor(fill = "#f0f5f9") +
  shadow_mark(past = TRUE, alpha = 1, fill = "#3a6589")

##Render animation
animate(plot_anim, end_pause = 60,
        height = 200, width = 400) # a higher res img would not upload here :(

# save animated map
#anim_save("animated_map.gif")
}