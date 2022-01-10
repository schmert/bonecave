#.....................................................
# Carl Schmertmann
# 11 Jan 2022
# 
# tile map of US states + circles with areas 
# proportional to 2020 Census populations
# 
# pop data is from 
# https://data.census.gov/cedsci/table?hidePreview=true&tid=PEPPOP2021.NST_EST2021_POP
#
# I created the tile "grid" in US-state-tile-info.csv 
# manually, by copying someone else's arrangment and then 
# indexing locations with integers: (0,0) for SW 
# corner [Hawaii], (10,7) for NE corner [Maine], etc.
# 
# The calculations use the integer locations of the 
# states as the bottom left corner of each state's 
# rectangle
#.....................................................

library(tidyverse) 

# read tile locations and populations ----
US_tiles = read_csv('US-state-tile-info.csv')
US_pop = read_csv('US-population-data.csv')

US = full_join(US_tiles, US_pop)

# generate XY data ----
# population circles centered on middle of each
# state's rectangle

make_circle = function(xx,yy,rr, n=40) {
  theta = seq(0, 2*pi, length.out=n)
  tibble(x = xx + rr * cos(theta),
         y = yy + rr * sin(theta))
}

# calculate a radius scaling factor K so that
# the area of each state's circle will be proportional 
# to its 2020 Census Population, and so that the 
# biggest radius equals an arbitrarily chosen "max_radius"
#
# pi * R^2 will equal population * K^2
# so K = max_radius * sqrt(pi/max_pop)

max_radius   = 1.1
max_pop      = max( US$census_Apr2020)
K            = sqrt( pi/max_pop) * max_radius 

circles = tibble()

for (i in 1:nrow(US)) {
  tmp = make_circle( US$grid_x[i] + 0.5,
                     US$grid_y[i] + 0.5,
                     rr = K * sqrt( US$census_Apr2020[i] / pi)) %>% 
        mutate(abb      = US$abb[i],
               name     = US$name[i],
               division = US$division[i],
               region   = US$region[i])
  
  circles = bind_rows(circles, tmp)
}

# create labels with population summaries ----

US$state_labels = (US$census_Apr2020 /1e6) %>% 
  sprintf("%.1f",.) %>% 
  paste0(US$abb,"\n",.,'M')

# construct tile map and add population circles ----

h = .025  # vert and horiz spacing between tiles will be 2h

TILEMAP = 
  ggplot(data=US) +
  aes(x=grid_x, y=grid_y, fill=region, label=state_labels) +
  geom_rect(aes(xmin=grid_x+h, xmax=grid_x + 1-h,
                ymin=grid_y+h, ymax=grid_y+1-h), color='grey', fill=NA) +
  geom_polygon(data=circles, aes(x=x,y=y, group=abb, fill=region),
               alpha=.70, inherit.aes = FALSE, color='grey20') +
  geom_text(nudge_x = .50, nudge_y = .50, size=5) +
  theme_void() +
  theme(plot.title   = element_text(hjust = 0.5, size=24, face='bold'),
        plot.caption = element_text(size=9, hjust=0.5)) +
  labs(title='US State Populations, 2020 Census',
       fill='REGION',
       caption='Source: US Census Bureau, https://tinyurl.com/2p83ebmk')

TILEMAP

# save the plot ----
ggsave(filename='US-tiles.png', width=11, height=8, dpi=300)
