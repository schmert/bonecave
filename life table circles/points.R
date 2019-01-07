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

D = rbind(D,
         expand.grid(country='radix', 
                     age=0:110, 
                     lx=100000))


D$x = 0
D$y = 0


D = D %>%
     arrange(age,country)

G = ggplot(data=D,aes(x=x,y=y, group=country, alpha=country,color=country)) +
        lims(x=c(-320,320), y=c(-320,320)) +
        geom_point(size=sqrt(D$lx)/2, shape=16) +
        coord_fixed() +
        labs(title=paste0('Survivors to age {frame_time} \n[Outer Circle = 100,000 male births]'),
             caption='Source: Human Mortality Database http://mortality.org\n2016 mortality rates',
             fill = 'Country') +
        theme_no_axes() +
        scale_color_manual(values=c(grey(.90),'gold','red')) +
        scale_alpha_manual(values=c(NA,.30,.10)) +
        transition_time(age)

movie = animate(G, fps=4)

anim_save('compare-life-tables-as-circles.gif', 
          animation=movie )


