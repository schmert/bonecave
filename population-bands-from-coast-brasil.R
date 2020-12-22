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

state_map = read_state(code_state = 'all') %>% 
  st_transform(29101)

BR = read_country() %>% 
  st_transform(29101)   


## construct the eastern edge of the country
## (slightly clumsy, but will work)

tmp = filter(state_map, name_state != 'Roraima')

xmin = st_bbox(tmp)['xmin']
xmax = st_bbox(tmp)['xmax']

ymin = st_bbox(tmp)['ymin']
ymax = st_bbox(tmp)['ymax']

yvals = seq(ymin, ymax, length.out=200)

easternmost = NA*yvals

for (i in seq(yvals)) {
  pts = matrix(c(xmin,yvals[i],  xmax,yvals[i]), ncol=2, byrow=TRUE)  
  stripe = st_linestring(pts) %>% 
    st_sfc() %>% 
    st_set_crs(29101)
  
  box = st_intersection(stripe,tmp) %>% 
    st_bbox()
  
  easternmost[i] = box['xmax']
  
}

# build eastern edge (this will include some S. Atlantic
# islands, but that won't affect calculations involving
# overlaps with municipios)

pts = cbind(easternmost, yvals)
edge = st_linestring(pts) %>% 
        st_sfc() %>% 
        st_set_crs(29101)


##

D$area = st_area(D$geometry) %>% 
            as.numeric()

popdata = read_csv(file='brasil-municipal-population-estimates-2019.csv',
                   skip=2)

D = inner_join(D, popdata, by=c('code_muni'='municode')) %>% 
      rename(estimate = pop)


# population at a given distance from 
# the "edge" (distance in meters!)
calcPop = function(this_distance) {
  
  print(paste(this_distance/1000, 'km'))
  
  buff      = st_buffer(edge, this_distance)
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
  
  return(pop)
}

distance_vals = c(seq(0,50,10),
                  seq(100,1000,50), 
                  seq(1050,3050,250)) * 1000

pop_vals = sapply(distance_vals,calcPop) 


Nzones = 5
pop_targets = sum(D$estimate) * seq(Nzones)/Nzones

target_distances = approx(x=pop_vals, y=distance_vals, xout=pop_targets)$y

## diagnostic plot

plot( distance_vals/1000, pop_vals, xlab='km',ylab='pop')
abline(h=sum(D$estimate),lwd=2)
abline(h=pop_targets, col='red', lty='dashed')
abline(v=target_distances/1000, col='purple')




hue = rep(c('darkgreen','gold','ivory'),50)

this_map = ggplot() +
             geom_sf(data=BR, fill=NA, color=NA) +
             theme_carl() +
             labs(title=paste('Brazil:',Nzones,'areas of equal population'),
                  caption=paste('Source: IBGE', 'https://www.ibge.gov.br/estatisticas/sociais/populacao/9103-estimativas-de-populacao.html?=&t=resultados'))



k=1
this_B = st_buffer(edge, target_distances[1]) %>% 
          st_intersection(BR)

this_map = this_map +
  geom_sf(data=this_B, fill=hue[k], 
          color=NA,alpha=.60)

print(this_map)

for (k in 2:length(target_distances)) {
  this_d = target_distances[k]
  last_d = target_distances[k-1]
  
  B = st_buffer(edge,this_d) %>% 
    st_difference(st_buffer(edge, last_d)) %>% 
    st_intersection(BR) 
  
  this_map = this_map +
              geom_sf(data=B, fill=hue[k], 
                      color=NA,alpha=.60)

}


this_map = this_map +
            geom_sf(data=state_map, fill=NA, color='grey') 


print(this_map)

ggsave(filename=paste0(Nzones,'-population-bands-from-coast-brasil.png'), plot=this_map,
       dpi=300, width=8, height=8)
