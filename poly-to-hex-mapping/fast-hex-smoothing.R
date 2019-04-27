library(tidyverse)
library(rgdal)
library(sf)
library(sp)
library(spdep)
library(Matrix)

rm(list=ls())
graphics.off()

windows(record=TRUE)

# this_state_abb  = c('AL','SE','BA','PI','MA','CE','RN','PB','PE')
# this_state_long = c('ALAGOAS','SERGIPE','BAHIA','PIAUI','MARANHAO','CEARA','RIO GRANDE DO NORTE','PARAIBA','PERNAMBUCO')
this_state_abb   = c('RS','SC','PR','SP')
this_state_long  = c('RIO GRANDE DO SUL','SANTA CATARINA','PARANA','SAO PAULO')
this_admin_name  = c('Rio Grande do Sul','Santa Catarina','Paraná','São Paulo')

filestub = paste(this_state_abb, collapse='-')

nhex = 4000
  
########################################################
# get the geographical information and e0 estimates if necessary

if (!file.exists('muni.df.csv')) {
  file_url = 'http://topals-mortality.schmert.net/data/muni.df.csv'
  download.file(url = file_url, destfile = 'muni.df.csv', mode='wb')
}

if (!file.exists('e0_microregion_summary.csv')) {
  file_url = 'http://mortality-subregistration.schmert.net/data/e0_microregion_summary.csv'
  download.file(url = file_url, destfile = 'e0_microregion_summary.csv', mode='wb')
}

# filter out selected geography

geo = read.csv('muni.df.csv') %>% 
          filter(ufabb %in% this_state_abb)  %>%
          select(ufabb,munid, municode, muniname, microcode, microname) 

e = read.csv('e0_microregion_summary.csv') %>%
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
       filter(NAME2_ %in% this_state_long) %>%
       select('ID_', contains('NAME'), geometry) %>%
       mutate(municode = floor( as.numeric( as.character(ID_)) / 10)) %>%
       left_join(geo)

plot(municipality_map['e0'])

## microregion map              
microregion_map = municipality_map %>%
        group_by(microcode) %>%
        summarize(e0=mean(e0))

P = nrow(microregion_map)

plot(microregion_map['e0'])

ggplot(data=microregion_map, aes(fill=e0)) +
  geom_sf(color=NA) +
  scale_fill_viridis_c(option='C') 
  

########################################################

## make a single exterior boundary for the state

state_boundary = municipality_map %>%
                   group_by(NAME2_) %>%
                   summarize()
                   
plot(state_boundary)                   

# generate a grid of hexagons that covers the state
hex_points = spsample(as_Spatial(state_boundary),
                      type='hexagonal',
                      n=nhex)

hexagons = HexPoints2SpatialPolygons(hex_points) %>%
            st_as_sf()

H = nrow(hexagons)

plot(hexagons, col='red', border='white')

## examine overlaps
overlap_map = st_intersection(hexagons, microregion_map)

A = st_area(overlap_map)  # areas of overlapping (polygon,hexagon) pairs

plot(overlap_map['e0'], border='white')

## remove hexagons without neighbors (islands, etc.)



#-- speedup experiment here

## find the overlaps between microregion polygons and
## hexagons: W is a (#polygons x #hexagons) = PxH matrix
## with row sums=1, such that 
## W %*% [hexagon values] = [polygon values]

# candidates will be a matrix of (microregion #, hexagon #) combinations
# that have some overlap. In most cases the hexagon will be entirely inside
# the microregion polygon

Imat = as.matrix(st_intersects(microregion_map, hexagons))
candidates = which(Imat, arr.ind = TRUE)
colnames(candidates) = c('microregion','hexagon')

W = matrix(0, P, H)
W[candidates] = A
W = prop.table(W,1)

###############################

polygon_values = microregion_map$e0

## make quadratic penalty matrix for the hexagons,
## based on adjacencies

adj     = poly2nb(hexagons)
K       = matrix(0, H, H)
diag(K) = sapply(adj, length)

for (i in seq(adj)) { K[i, adj[[i]]] = -1 }


## use sparse matrices to solve the quadratic programming problem:
##    min z'Kz st Wz = polygon_values
## where z'Kz is the sum of sq diffs between adjacent hex values
##
## This will find the smoothest hexmap that exactly matches the
## polygon values when averaged by area

A = Matrix( rbind( cbind( 2*K, -t(W)), 
                   cbind(   W, diag(0,P)) ) )
b = Matrix( c(rep(0,H), polygon_values), ncol=1)

optimal_hex_values = (solve(A,b))[1:H]

hexagons$e0 = optimal_hex_values

## plot the smoothest hex map

polymap = 
  ggplot(data=microregion_map, aes(fill=e0)) +
  geom_sf(color=NA) +
  scale_fill_viridis_c(limits=range(optimal_hex_values)) +
  labs(title=this_state_long)  +
  theme_minimal()

print(polymap)

ggsave(file=paste0(filestub,'1.png'), dpi=300)

hexmap = 
  ggplot( data=hexagons, aes(fill=e0)) +
  geom_sf(color=NA) +
  scale_fill_viridis_c(limits=range(optimal_hex_values)) +
  labs(title=this_state_long)  +
  theme_minimal()

print(hexmap)

ggsave(file=paste0(filestub,'2.png'), dpi=300)

hexmap + geom_sf(data=microregion_map, color='grey',fill=NA)

ggsave(file=paste0(filestub,'3.png'), dpi=300)

## add cities?

cities = read.csv('worldcities.csv', encoding='UTF-8', stringsAsFactors = FALSE) %>%
            filter(iso2=='BR', admin_name %in% this_admin_name,
                   population > 100000) %>%
            st_as_sf( coords=c('lng','lat'))

hexmap + geom_sf(data=cities,inherit.aes = FALSE, shape=1, 
                 color='red', size=2.5)

ggsave(file=paste0(filestub,'4.png'), dpi=300)




