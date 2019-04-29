library(tidyverse)
library(rgdal)
library(sf)
library(sp)
library(spdep)
library(Matrix)
library(igraph)

rm(list=ls())
graphics.off()

windows(record=TRUE)

# this_state_abb  = c('AL','SE','BA','PI','MA','CE','RN','PB','PE')
# this_state_long = c('ALAGOAS','SERGIPE','BAHIA','PIAUI','MARANHAO','CEARA','RIO GRANDE DO NORTE','PARAIBA','PERNAMBUCO')
#this_state_abb   = c('BA','ES','RJ','MG','RS','SC','PR','SP')
#this_state_long  = c('BAHIA','ESPIRITO SANTO','RIO DE JANEIRO','MINAS GERAIS','RIO GRANDE DO SUL','SANTA CATARINA','PARANA','SAO PAULO')

this_state_abb  = c('MT','MS','TO','GO','DF',
                    'MA','PA','AM','AC','RO',
                    'RR','AP','CE','PI','RN',
                    'PE','PB','BA','SE','AL',
                    'MG','ES','SP','RJ','PR',
                    'SC','RS')
this_state_long = c('MATO GROSSO','MATO GROSSO DO SUL',
                    'TOCANTINS','GOIAS','DISTRITO FEDERAL',
                    'MARANHAO','PARA','AMAZONAS',
                    'ACRE','RONDONIA','RORAIMA','AMAPA',
                    'CEARA','PIAUI','RIO GRANDE DO NORTE',
                    'PERNAMBUCO','PARAIBA','BAHIA','SERGIPE',
                    'ALAGOAS','MINAS GERAIS',
                    'ESPIRITO SANTO','SAO PAULO','RIO DE JANEIRO',
                    'PARANA','SANTA CATARINA','RIO GRANDE DO SUL') 

filestub = paste(this_state_abb, collapse='-')

nhex = 10000
  
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

## construct an adjacency list for the hexagons,
## SUCH THAT THERE ARE NO DISJOINT SUBGRAPHS (=unconnected 
##   hexagons or sets of hexagons)
adj     = poly2nb(hexagons)

agraph = graph_from_adj_list(adj, mode='total')   # an igraph

disjoint = !is_connected(agraph)
if (disjoint) {
  cl  = clusters(agraph)
  ncl = cl$no
  icl = cl$membership
  
  # for each pair of subgraphs, find the pair of member hexagons
  # that are closest and add an artificial adjacency (think of this
  # as adding a ferry between islands)
  print(paste('-----',ncl,'disjoint subgraphs of hexagons'))
  print(table( icl ))
  
  for (i in 1:(ncl-1)) {
    for (j in (i+1):ncl) {
      g1 = hexagons[icl==i,] %>% mutate(hexnum=which(icl==i))
      g2 = hexagons[icl==j,] %>% mutate(hexnum=which(icl==j))

      dist = st_distance(g1,g2)
      ix   = as.numeric( which( dist==min(dist), arr.ind=TRUE) )
      ii   = g1$hexnum[ix[1]]
      jj   = g2$hexnum[ix[2]] 
      # (ii,jj) are the closest pair: add an adjacency
      print(paste('-- adding adjacency',ii,'<>',jj))
      tmp = c(adj[[ii]], jj)
      adj[[ii]] = tmp[tmp!=0]  # remove any "0"s from poly2nb
      tmp = c(adj[[jj]], ii)
      adj[[jj]] = tmp[tmp!=0]  # remove any "0"s from poly2nb
    }
  }
} # if disjoint


## make quadratic penalty matrix for the hexagons,
## based on adjacencies

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
  scale_fill_viridis_c(limits=range(optimal_hex_values, option='C')) +
  labs(title=filestub)  +
  theme_minimal()

print(polymap)

ggsave(file=paste0(filestub,'1.png'), dpi=300)

hexmap = 
  ggplot( data=hexagons, aes(fill=e0)) +
  geom_sf(color=NA) +
  scale_fill_viridis_c(limits=range(optimal_hex_values), option='C') +
  labs(title=filestub)  +
  theme_minimal()

print(hexmap)

ggsave(file=paste0(filestub,'2.png'), dpi=300)

hexmap + geom_sf(data=microregion_map, color='grey',fill=NA)

ggsave(file=paste0(filestub,'3.png'), dpi=300)

## add cities?

this_bbox = st_bbox(hexagons)  # bounding box for the hexagons

# select any world cities that are in the bounding box and have
# at least 500,000 residents

cities = read.csv('worldcities.csv', encoding='UTF-8', stringsAsFactors = FALSE) %>%
            filter(population > 500000) %>%
            st_as_sf( coords=c('lng','lat')) %>%
            st_crop(this_bbox)

# add the cities to the map as circles
hexmap + geom_sf(data=cities,inherit.aes = FALSE, shape='X', 
                 color='white', size=4) +
         geom_sf(data=state_boundary, lwd=0.5, fill=NA,color='grey', inherit.aes = FALSE)

ggsave(file=paste0(filestub,'4.png'), dpi=300)




