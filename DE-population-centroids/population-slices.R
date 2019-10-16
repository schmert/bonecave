library(tidyverse)
library(data.table)
library(geobr)
library(sf)

#--------------------------------------------
# map data downloaded from https://gadm.org
#--------------------------------------------

# state-level map, 16
state_map = readRDS('gadm36_DEU_1_sf.rds')

# Kreis-level map, 403 regions, 1 of which (Bodensee) is
# a water body that can be removed
kreis_map = readRDS('gadm36_DEU_2_sf.rds') %>%
              filter(TYPE_2 != 'Water body') %>%
              mutate(AGS = as.numeric(CC_2))

#--------------------------------------------
# 31 Dec 2017 population data, downloaded from 
# http://regionalstatistik.de
#
# Table 12411-01-01-4	Bevölkerung nach Geschlecht - Stichtag 31.12. - 
#   regionale Tiefe: Kreise und krfr. Städte
#--------------------------------------------

pop = read.csv2('12411-01-01-4.csv', skip=8,header=FALSE,
                col.names=c('code','popname','total_pop','male_pop','female_pop')) %>%
          mutate(AGS = as.numeric(as.character(code)),
                 total_pop = as.numeric(total_pop)) %>%
          select(popname,AGS,total_pop)

## tweak codes for city-states
iHamburg = which(pop$AGS == 2)
pop$AGS[iHamburg] = 2000

iBerlin = which(pop$AGS==11)
pop$AGS[iBerlin] = 11000

#--------------------------------------------
# merge population totals onto map
#--------------------------------------------

kreis_map = left_join( kreis_map , pop, by='AGS') %>%
              mutate(centroid = st_centroid(geometry))

             

pop.df = data.frame( st_coordinates(kreis_map$centroid),
                     name = kreis_map$NAME_2,
                     AGS  = kreis_map$AGS,
                     pop  = kreis_map$total_pop)

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
nslice     = 4

main_map = ggplot(data=state_map) +
            geom_sf(fill='ivory', color='black', lwd=0.30) +
            theme_bw() +
            theme(panel.border = element_blank(),
                  axis.text    = element_blank(),
                  axis.title   = element_blank(),
                  axis.ticks   = element_blank(),
                  panel.grid   = element_line(color='transparent'),
                  plot.title   = element_text(size=14, hjust=0.5, face='bold'),
                  plot.caption = element_text(size=9, hjust=0.5, face='bold')) +
            labs(title   =paste(nslice,'Zones of Equal Population, Germany 2017'),
                 caption ='Source: regionalstatistik.de, Tabelle 12411-01-01-4 data for 31 Dec 2017')


for (this_vname in c('X','Y')) {

  divisor     = rep(NA,nslice-1)

  for (i in seq(divisor)) {
    divisor[i] = pop_quantile( this_vname,  this_prob= i/nslice )
  }


  ## this next part is very convoluted and inelegant...

  ## make an sf object that has the part of each "strip"
  ## that actually falls inside Germany and overlay it on the map

  ## construct data frame with the corners of rectangles
  ## corresponding to slices

  bb = st_bbox(state_map)

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
    st_crs(tmp) = st_crs(state_map)
    this_slice = st_intersection(tmp,state_map)

    G = G + geom_sf(data=this_slice, color=NA,
                    fill=c('gold','red','black')[1+ (i-1) %% 3],
                    alpha=.40)
  }

  ggsave(file=paste0('Germany-strips-',this_vname,'-',nslice,'.png'),
         plot=G,
         height=8,width=8,units='in',dpi=300)

} # this_vname