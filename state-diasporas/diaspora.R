#.....................................................
# Carl Schmertmann
# 17 Mar 2022
# 
# pick a state; plot the locations of its natives
# using dots placed randomly in state tiles
# 
# Input data comes from ipums.org online data analysis
# tool; details at the top of the .csv file
#.....................................................

library(tidyverse)

# read FL data and separate state names and fipscodes
FL = read.csv(file='FL-natives.csv', skip=11, header=TRUE,
              col.names = c('var','birthplace','fipscode','male','female','total'))

# read the tile map information
US_tiles = read_csv('US-state-tile-info.csv')

# join the two datasets and keep the important variables
map = FL %>% 
        left_join(US_tiles, by='fipscode') %>% 
        select(abb,name,fipscode,male,female,total,grid_x,grid_y)

# scale so that the state with the smallest
# population of FL natives gets a single dot

dotpop  = min(map$total)

dotpop_text = as.character( round(dotpop,-2 ))

# calculate an integer number of dots for each state of residence

map = map %>% 
       mutate(ndots = floor(total/dotpop))
  
# create a new dataframe with random dot positions
# for a state with lower left corner (x,y) the dots
# will be randomly positioned in [x+.1,x+.9] x [y+.1,y+.9]

dots = tibble()

for (i in 1:nrow(map)) {
  
  this_n = map$ndots[i]
  this_x = map$grid_x[i]
  this_y = map$grid_y[i]
  
  tmp = tibble(
          x = this_x + runif(this_n, min=.10, max=.90),
          y = this_y + runif(this_n, min=.10, max=.90),
  )
  
  dots = bind_rows( dots,
                    tmp)
}

h = .025  # vert and horiz spacing between tiles will be 2h

TILEMAP = 
  ggplot(data=map) +
  aes(x=grid_x, y=grid_y, label=abb) +
  geom_rect(aes(xmin=grid_x+h, xmax=grid_x + 1-h,
                ymin=grid_y+h, ymax=grid_y+1-h), color='grey', fill=NA) +
  geom_point(data=dots, aes(x=x,y=y), inherit.aes = FALSE,
             color='orangered', alpha=.40, size=1.5) +
  geom_text(nudge_x = .50, nudge_y = .50) +
  theme_void() +
  theme(title=element_text(size=20),
        plot.subtitle = element_text(size=14)) +
  labs(title='Where do Florida-born Adults Live?',
       subtitle = paste('Each dot represents approx', 
                        dotpop_text,'Floridians 25+ years old'))
  

# save the plot ----
ggsave(filename='FL-diaspora.png', width=11, height=8, dpi=300, bg='white')

