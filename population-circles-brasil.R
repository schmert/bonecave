library(tidyverse)
library(tidycensus)
library(sf)
library(units)
library(showtext)
library(geobr)


font_add_google("Roboto", "Roboto")
showtext_auto() 

  
theme_carl <- function () { 
  theme_minimal(base_size=13, base_family="Roboto") %+replace% 
    theme(plot.title=element_text(face='bold',size=90),
          plot.caption=element_text(size=30),
          axis.text = element_blank(),
          panel.grid = element_blank())
}

# read muni data, rename column with geometry, and transform
# to planar SAD69 coordinates (29101)

D = read_municipality(code_muni = 'all', simplified = TRUE) %>% 
     rename(geometry = geom) 

orig_crs = st_crs(D)

D = D %>% 
     st_transform(29101)  %>% 
     st_buffer(dist=0.0) 


## lon/lat for possible origin points
GIG   = c(-43.2566, -22.8053) 
BELEM = c(-48.504,   -1.456)
MANAUS = c(-60.046, -3.036)
GRU   = c(-46.473, -23.431)

# select origin
origin = st_point(GRU) %>% 
       st_sfc() %>% 
       st_set_crs(orig_crs) %>% 
       st_transform(29101)

D$area = st_area(D$geometry) %>% 
            as.numeric()

popdata = read_csv(file='brasil-municipal-population-estimates-2019.csv',
                   skip=2)

D = inner_join(D, popdata, by=c('code_muni'='municode')) %>% 
      rename(estimate = pop)


# population at a given distance from 
# a selected center (distance in meters!)
P = function(this_center, this_distance) {
  print(this_distance)
  
  buff      = st_buffer(this_center, this_distance)
  overlaps  = st_intersects(D$geometry, buff) 
  sel       = which(sapply(overlaps, length) == 1)
  asel      = st_intersection(D$geometry[sel],buff) %>%   
               st_area()
  
  df_sel = tibble( code_muni    = D$code_muni[sel], 
                   overlap_area = as.numeric(asel)) %>% 
           left_join(select(D,code_muni,name_muni,uf,estimate,area),
                     by='code_muni') %>% 
           mutate(prop_overlap = overlap_area/area)
  
  pop = sum( df_sel$prop_overlap * df_sel$estimate)
  
if (FALSE) {

  print(
    list(
        this_distance_in_km = this_distance/1000,
        number_of_overlaps  = length(sel),
        range_prop_overlap = range(prop_overlap),
        total_sel_pop      = sum(D$estimate[sel]),
        estimated_pop      = pop)
    )
  
  
  M = ggplot() + 
       geom_sf( data = D$geometry[sel,]) +
       geom_sf( data = buff, fill='slateblue', alpha=.30) +
       labs(title=paste(this_distance/1000, 'km'))

  if (any(prop_overlap > 1)) {
    bad = which(prop_overlap > 1) 
    print(list(bad_obs = sel[bad],
               prop_overlap = prop_overlap[bad]))
    
    M = M + geom_sf(data=D$geometry[sel[bad],], fill='red')
  }
  
  print(M)
}  
  
  return(pop)
}

distance_vals = seq(50,4000,150) * 1e3

pop_vals = sapply(distance_vals, 
                  function(d) P(this_center=origin, this_distance = d))

Ncircles = 10
pop_targets = sum(D$estimate) * seq(Ncircles)/Ncircles

target_distances = approx(x=pop_vals, y=distance_vals, xout=pop_targets)$y


## diagnostic plot

plot( distance_vals/1000, pop_vals, xlab='km',ylab='pop')
abline(h=sum(D$estimate),lwd=2)
abline(h=pop_targets, col='red', lty='dashed')
abline(v=target_distances/1000, col='purple')

hue = rep(c('darkgreen','white','gold'),50)

state_map = read_state(code_state = 'all') %>% 
              st_transform(29101)

BR = read_country() %>% 
  st_transform(29101)   


this_map = ggplot() +
             geom_sf(data=BR, fill=NA) +
             theme_carl() +
             labs(title=paste('Brazil:',Ncircles,'areas of equal population'),
                  caption=paste('Source: IBGE', 'https://www.ibge.gov.br/estatisticas/sociais/populacao/9103-estimativas-de-populacao.html?=&t=resultados'))



k=1
this_B = st_buffer(origin, target_distances[1]) %>% 
          st_intersection(BR)

this_map = this_map +
  geom_sf(data=this_B, fill=hue[k], 
          color=NA,alpha=.60)

print(this_map)

for (k in 2:length(target_distances)) {
  this_d = target_distances[k]
  last_d = target_distances[k-1]
  
  B = st_buffer(origin,this_d) %>% 
    st_difference(st_buffer(origin, last_d)) %>% 
    st_intersection(BR) 
  
  this_map = this_map +
              geom_sf(data=B, fill=hue[k], 
                      color=NA,alpha=.60)

}

xx = st_coordinates(origin)[,'X']
yy = st_coordinates(origin)[,'Y']

this_map = this_map +
            geom_sf(data=state_map, fill=NA) +
            geom_sf(data=origin,
                       shape='+',
                       color='blue',
                       size=10)

print(this_map)

ggsave(filename='population-circles-brasil.png', plot=this_map,
       dpi=300, width=8, height=8)
