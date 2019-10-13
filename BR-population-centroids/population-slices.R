library(tidyverse)
library(data.table)
library(geobr)
library(sf)

# If the Brazil state map is already read,
# use it. Otherwise read and save. 

if (file.exists('BR.map.Rdata')) {
    load('BR.map.Rdata')  
} else {
  BR = read_state(code_state='all', year=2010)
  save(BR, file='BR.map.Rdata')
}

# If municipal population totals and centroids are already calculated,
# use them. Otherwise calculate and save. 

if (file.exists('pop.df.Rdata')) { 
      load('pop.df.Rdata') 
  } else {

  # get municipal-level 2010 census data from a previous project
  muni_info = fread('http://schmert.net/topals-mortality/data/muni.df.csv') %>%
                 select(munid, municode, muniname, ufabb)
  
  pop_data = fread('http://schmert.net/topals-mortality/data/big2010.df.csv') %>%
                left_join(muni_info) %>% 
                group_by(municode, muniname, ufabb) %>%
                summarize(pop=sum(pop))
  
  # the map returned by geobr has 2 extra municipalities,
  # Lagoa Mirim & Lagoa dos Patos in Rio Grande do Sul
  # I'm fairly certain these water areas with zero population
  #
  # read_municipality returns 7-digit values for code_muni
  # the first 6 of these match the municode value in
  # muni_info and pop_data
  
  map = read_municipality(code_muni = 'all', year=2010)
  
  ## join map data to pop data and calculate centroids
  map = map %>%
          mutate(municode = floor(code_muni/10)) %>%
          right_join(pop_data, by='municode') %>%
          mutate(centroid = st_centroid(geometry))
  
  pop.df = data.frame( st_coordinates(map$centroid),
                       municode = map$municode,
                       muniname = map$name_muni,
                       uf       = map$abbrev_state,
                       pop = map$pop)
  
  save(pop.df, file='pop.df.Rdata')
} # else


## calculate population quantiles in either the X or Y direction

pop_quantile = function(vname, this_prob) {
  this_var   = pop.df[[vname]]
  N          = pop.df$pop
  coord_vals = seq(min(this_var), max(this_var), length.out=1001)
  pvals = sapply(coord_vals, function(z) sum(N[this_var < z])/sum(N))

  qq   = approxfun(x=pvals, y=coord_vals)
  return( qq(this_prob) )
}


## construct the main map

main_map = ggplot(data=BR) +
            geom_sf(fill='ivory', color='darkgreen', lwd=0.30) +
            theme_bw() +
            theme(panel.border = element_blank(),
                  axis.text    = element_blank(),
                  axis.title   = element_blank(),
                  axis.ticks   = element_blank(),
                  panel.grid   = element_line(color='transparent'),
                  plot.title   = element_text(size=14, hjust=0.5, face='bold'),
                  plot.caption = element_text(size=12, hjust=0.5, face='bold')) +
            labs(title   =paste(nslice,'Zones of Equal Population, Brazil 2010'),
                 caption ='Source: IBGE 2010 Censo Demográfico')
      

for (this_vname in c('X','Y')) {
  

  nslice     = 10
  
  divisor     = rep(NA,nslice-1)
  
  for (i in seq(divisor)) {
    divisor[i] = pop_quantile( this_vname,  this_prob= i/nslice )
  }
  

  ## this next part is very convoluted and inelegant...  

  ## make an sf object that has the part of each "strip"
  ## that actually falls inside Brazil, and overlay it on the map
  
  ## construct data frame with the corners of rectangles
  ## corresponding to slices
  
  bb = st_bbox(BR)
  
  itsX = rep(this_vname=='X', nslice)
  
  rectangles.df = data.frame(
    x1 = ifelse(itsX, 
                yes = c(bb$xmin, divisor),
                no  = rep(bb$xmin, nslice)),
    x2 = ifelse(itsX, 
                yes = c(divisor, bb$xmax),
                no  = rep(bb$xmax, nslice)),
    y1 = ifelse(itsX, 
                no = c(bb$ymin, divisor),
                yes  = rep(bb$ymin, nslice)),
    y2 = ifelse(itsX, 
                no = c(divisor, bb$ymax),
                yes  = rep(bb$ymax, nslice))
  )
  
  G = main_map
  
  for (i in 1:nrow(rectangles.df)) {
    rw  = rectangles.df[i,]
    tmp = list( rbind( c(rw$x1,rw$y1), 
                       c(rw$x1,rw$y2),
                       c(rw$x2,rw$y2),
                       c(rw$x2,rw$y1),
                       c(rw$x1,rw$y1) ) ) 
    
    tmp = st_sfc( st_polygon(tmp))  
    st_crs(tmp) = st_crs(BR)  
    this_slice = st_intersection(tmp,BR)
    
    G = G + geom_sf(data=this_slice, color=NA, 
                    fill=c('darkgreen','gold')[1+ i %% 2], 
                    alpha=.40)
  }
  
  ggsave(file=paste0('Brazil-strips-',this_vname,'-',nslice,'.png'),
         plot=G,
         height=8,width=8,units='in',dpi=300)
  

} # this_vname