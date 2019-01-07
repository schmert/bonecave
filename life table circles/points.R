# devtools::install_github('thomasp85/gganimate')
library(tidyverse)
library(ggforce)
library(data.table)
library(gganimate)


graphics.off()
windows(record=TRUE)

D = fread('USA SWE male 2016 life tables.txt') %>%
       as.data.frame() %>%
       select(country, age, lx) 

D$age   = 0:110    # make ages numeric
D$radix = 100000   # add radix column
D$x = D$y = 0

## reorganize (wide) 
D = D %>% 
  spread(key=country, value=lx)

G = ggplot(data=D,aes(x=x,y=y)) +
        lims(x=c(-320,320), y=c(-320,320)) +
        geom_point(size=sqrt(D$radix)/2, shape=1,color='black') +
        geom_point(size=sqrt(D$SWE)/2, shape=16,color='gold', alpha=.30) +
        geom_point(size=sqrt(D$USA)/2, shape=16,color='red',  alpha=.10) +
        coord_fixed() +
        labs(title=paste0('Survivors to age ',D$age,'\n[Outer Circle = 100,000 male births]'),
             caption='Source: Human Mortality Database http://mortality.org\n2016 mortality rates',
             fill = 'Country') +
        theme_no_axes() +
        scale_fill_manual(values=c('gold','red' )) +
        transition_states(age)

movie = animate(G, fps=4)

anim_save('compare-life-tables-as-circles.gif', 
          animation=movie )

