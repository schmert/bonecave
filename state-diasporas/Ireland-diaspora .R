#.....................................................
# Carl Schmertmann
# 17 Mar 2022 (St Patrick's Day!)
# 
# pick a state; plot the locations of Irish natives
# using dots placed randomly in state tiles
# 
# Input data comes from ipums.org online data analysis
# tool; details at the top of the .csv file
#.....................................................

library(tidyverse)
library(ggimage)

# read FL data and separate state names and fipscodes
IRE = read.csv(file='Irish-born.csv', skip=11, header=TRUE,
              col.names = c('var','birthplace','fipscode','male','female','total'))




# read the tile map information
US_tiles = read_csv('US-state-tile-info.csv')

# join the two datasets and keep the important variables
map = IRE %>% 
        right_join(US_tiles, by='fipscode') %>% 
        select(abb,name,fipscode,male,female,total,grid_x,grid_y)

# scale so that the state with the smallest
# population of FL natives gets a single dot

dotpop  = 100

dotpop_text = "100" 

# calculate an integer number of dots for each state of residence

map = map %>% 
       mutate(ndots = pmax(1,floor(total/dotpop)))
  
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
#  geom_point(data=dots, aes(x=x,y=y), inherit.aes = FALSE,
#             color='green', alpha=.40, size=1.5) +
  geom_image(data=dots, aes(x=x,y=y), inherit.aes = FALSE,
             image='shamrock.png', size=0.015, alpha=.20) +
  geom_text(nudge_x = .50, nudge_y = .50, size=5) +
  theme_void() +
  theme(title=element_text(size=20, color='darkgreen'),
        plot.subtitle = element_text(size=14),
        text = element_text(face='bold')) +
  labs(title='Where do Irish-born People Live in the USA?',
       subtitle = paste('Each dot represents approx', 
                        dotpop_text,'natives of Ireland (ACS 2015-2019 data)'),
       caption='Source: IPUMS-USA Online Analysis tool, https://ipums.org  [@cschmert]  ')
  

# save the plot ----
ggsave(filename='Irish-diaspora.png', width=11, height=8, dpi=300, bg='white')

