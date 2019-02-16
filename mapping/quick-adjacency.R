library(rgdal, quietly=TRUE)
library(spdep, quietly=TRUE)
library(tidyverse, quietly=TRUE)
library(mapproj, quietly=TRUE)

#---------------------------------------------
# download and process shapefile data
# for a selected country and administrative
# level (e.g. Angolan Municipalities)
#---------------------------------------------

# choose country and level
country_code = 'AGO'  # USA, GBR,... 
admin_level  = 2      # 0,1,2,3



# get the file from gadm.org and load it into R
download.file(url=paste0('https://biogeo.ucdavis.edu/data/gadm3.6/Rsp/gadm36_',
                         country_code,'_',admin_level,
                         '_sp.rds'),
              mode='wb',
              destfile='this_shapefile.rds')

this_map = readRDS(file='this_shapefile.rds')

# calculate adjacencies
adjacency = this_map %>%
              poly2nb() %>%
              nb2mat(style='B', zero.policy = TRUE)

#---------------------------------------------
#  make a nice map of the regions and 
#  adjacencies
#---------------------------------------------

map_df = fortify(this_map, regions='NAME_1')

centroids_df = coordinates(this_map) %>%
                data.frame() %>%
                rename(long=X1, lat=X2)

nreg = length(this_map)

# construct a data frame in which each row is a unique
# adjacent pair of regions

edge_df = data.frame()

for (i in 1:nreg) {
  for (j in i:nreg) {
    if (adjacency[i,j]==1) {
      tmp = data.frame(long1 = centroids_df$long[i],
                       lat1  = centroids_df$lat[i],
                       long2 = centroids_df$long[j],
                       lat2  = centroids_df$lat[j])
      edge_df = rbind(edge_df,tmp)
    } # if
  } # j
} # i


ggplot( data=map_df, aes(x=long,y=lat, group=group)) +
      geom_polygon(color='black',alpha=.60, fill=NA) +
      geom_point(data=centroids_df, aes(x=long,y=lat),
                 color='orangered',inherit.aes = FALSE) +
      geom_segment(data=edge_df, aes(x=long1,y=lat1,
                                     xend=long2,yend=lat2),
                   color='red', inherit.aes = FALSE) +
      labs(title=paste(country_code,'Admin Level',admin_level,
                       '-', nrow(adjacency),'Units')) +
      theme_minimal() +
      coord_map()


ggsave(file='quick-adjacency.png',
       width=8, height=8, units='in', dpi=300)

