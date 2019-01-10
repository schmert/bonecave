# devtools::install_github('thomasp85/gganimate')
library(tidyverse)
library(data.table)
library(gganimate)
library(grid)

graphics.off()
windows(record=TRUE)

# D = fread('USA SWE male 2016 life tables.txt') %>%
#        as.data.frame() %>%
#        select(country, age, lx) 
# 
# D$age   = 0:110    # make ages numeric
# D$radix = 100000   # add radix column
# 
# ## reorganize (wide) 
# D = D %>% 
#   spread(key=country, value=lx)
# 

D = fread('USA SWE male 2016 life tables.txt') %>%
  as.data.frame() %>%
  select(country, age, lx)

D$age   = 0:110    # make ages numeric

D = rbind(D,
          expand.grid(country='radix', 
                      age=0:110, 
                      lx=100000)) %>%
          arrange(age,country)

fpa = 4  # frames per age
aps = 3  # ages per second

G = ggplot(data=D,aes(x=0, y=0)) +
        geom_point(aes(size=lx, color=country, alpha=country)) +
        scale_size_area(guide = FALSE, max_size = 175) +
        coord_fixed() +
        labs(title=paste0('Survivors to age {(frame-1) %/% fpa}'),
             subtitle='[Outer Circle = 100,000 male births]',
             caption='Source: Human Mortality Database http://mortality.org\n2016 mortality rates',
             fill = 'Country') +
        theme_no_axes() +
        scale_color_manual(name='',values=c('lightgrey','gold','red')) +
        scale_alpha_manual(values=c(.30,.50,.25)) +
        guides(alpha=FALSE, 
               color = guide_legend(override.aes = list(size=10))) +
        transition_time(age) 

movie = animate(G, nframes=111*fpa, fps=aps*fpa)

anim_save('compare-life-tables-as-circles.gif', 
          animation=movie )

