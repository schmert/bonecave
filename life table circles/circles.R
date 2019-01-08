# devtools::install_github('thomasp85/gganimate')
library(tidyverse)
library(ggforce)
library(data.table)
library(gganimate)


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


G = ggplot(data=D,aes(group=country, 
                      color=country, 
                      fill =country,
                      alpha=country )) +
        geom_circle(aes(x0=0, y0=0, r=sqrt(lx) ),lwd=1,lty=1) +
        coord_fixed() +
        labs(title=paste0('Survivors to age ',D$age,'\n[Outer Circle = 100,000 male births]'),
             caption='Source: Human Mortality Database http://mortality.org\n2016 mortality rates',
             fill = 'Country') +
        theme_no_axes() +
        scale_fill_manual(values=c('gold','red','blue' )) +
        transition_reveal(age)

movie = animate(G, fps=4)

anim_save('compare-life-tables-as-circles.gif', 
          animation=movie )

