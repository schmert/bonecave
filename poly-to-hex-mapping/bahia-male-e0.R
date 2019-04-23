library(tidyverse)
library(rgdal)
library(sf)
library(sp)
library(spdep)

rm(list=ls())
graphics.off()

windows(record=TRUE)

# get geographical information from schmert.net
# make a df with microregion numbers and names
file_url = 'http://topals-mortality.schmert.net/data/muni.df.csv'

this_state_abb  = 'PA'
this_state_long = 'PARA'

geo = read.csv(file_url) %>% 
        filter(ufabb==this_state_abb)  %>%
        select(ufabb,munid, municode, muniname, microcode, microname) 


# get state e0 estimates from schmert.net
file_url = 'http://mortality-subregistration.schmert.net/data/e0_microregion_summary.csv'

e = read.csv(file_url) %>%
      filter(coverage_model=='partial',
             pctile==50, 
             sex=='m', 
             microcode %in% geo$microcode) %>%
      select(ufabb, microcode, sex, e0)

geo = left_join(geo, e)

########################################################
# get the shapefile data if necessary

prefix = 'http://mortality-subregistration.schmert.net/data/brasilmun2010_new'

for (suffix in c('.shp','.dbf','.shx')) {
  local_filename = paste0('brasilmun2010_new', suffix)
  if (!file.exists(local_filename)) {
    file_url       = paste0(prefix,suffix)
    download.file(url = file_url, destfile = local_filename, mode='wb')
  } # if
} # for 

########################################################

## municipio map
municipality_map = st_read(dsn='.', layer='brasilmun2010_new') %>%
       filter(NAME2_ == this_state_long) %>%
       select('ID_', contains('NAME'), geometry) %>%
       mutate(municode = floor( as.numeric( as.character(ID_)) / 10)) %>%
       full_join(geo)

plot(municipality_map['e0'])

## microregion map              
microregion_map = municipality_map %>%
        group_by(microcode) %>%
        summarize(e0=mean(e0))

P = nrow(microregion_map)

plot(microregion_map['e0'])

########################################################

## make a single exterior boundary for the state

state_boundary = municipality_map %>%
                   group_by(NAME2_) %>%
                   summarize(state=this_state_abb)
                   
plot(state_boundary['state'])                   

# generate a grid of hexagons that covers the state
hex_points = spsample(as_Spatial(state_boundary),
                      type='hexagonal',
                      n=2000)

hexagons = HexPoints2SpatialPolygons(hex_points) %>%
            st_as_sf()

H = nrow(hexagons)

plot(hexagons, col='red', border='white')

## peek at overlaps
tmp = st_intersection(hexagons, microregion_map)
plot(tmp['e0'], border='white')

overlap = function(p,h) {
  result = 0
  this_area = st_intersection( microregion_map[p,],
                               hexagons[h,])
  if (nrow(this_area) > 0) result = st_area(this_area)
  return(result)
}

grid = expand.grid( p=1:P, h=1:H, area=0)

for (i in 1:nrow(grid)) {
  this_poly = grid$p[i]
  this_hex  = grid$h[i]
  grid$area[i] = overlap(this_poly, this_hex)
}

W = prop.table( matrix(grid$area, nrow=P, ncol=H) , margin = 1)

###############################

v = microregion_map$e0

# eigen decomp of W'W
e = eigen( crossprod(W))

r = qr(W)$rank

X    = e$vectors[, 1:r]
Dinv = diag( 1/ e$values[1:r])
Z    = e$vectors[, -(1:r)]

gamma = Dinv %*% t(X) %*% t(W) %*% v  # fixed part of hex values

## make quadratic penalty matrix for the hexagons,
## based on adjacencies

adj     = poly2nb(hexagons)
K       = matrix(0, H, H)
diag(K) = sapply(adj, length)

for (i in seq(adj)) { K[i, adj[[i]]] = -1 }

## find the overlaps between microregion polygons and
## hexagons: W is a (#polygons x #hexagons) = PxH matrix
## with row sums=1, such that 
## W %*% [hexagon values] = [polygon values]


eps = -solve( t(Z) %*% K %*% Z, 
              t(Z) %*% K %*% X %*% gamma)

vhex = X %*% gamma + Z %*% eps

hexagons$e0 = vhex

ggplot( data=hexagons, aes(fill=e0)) +
  geom_sf(color=NA) +
  scale_fill_viridis_c(option='C') +
  labs(title=this_state_long)  +
  theme_minimal()






