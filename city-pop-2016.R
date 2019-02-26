###########################################
# largest metro areas
# UN 2016 data from
# https://www.thoughtco.com/the-largest-cities-in-the-world-4163437
###########################################

rm(list=ls())
library(tidyverse)
library('ggflags')

graphics.off()
windows(record=TRUE)

df = read.csv('city-pop-2016.csv', fileEncoding = 'UTF-8',
              stringsAsFactors = FALSE) %>%
      mutate(city_label = paste0(city,'-',country_code))

ggplot(data=df, aes(x=pop, y=reorder(city_label,pop),
                    country=short_code)) +
           geom_flag(size=7) +
           scale_country(guide=FALSE) +
           scale_x_continuous(limits=c(0,40000),
                              breaks=seq(0,40000,5000),
                              minor_breaks = NULL,
                              labels=paste0(seq(0,40,5),'Mil')) +
           labs(title='Population of the World\'s Largest Megacities, 2016',
                caption = 'Source: United Nations',
                x = 'Population in 2016',
                y = '') +
           theme_bw()

ggsave(file='city-pop-2016.png',
       width=11, height=8.5, units='in', dpi=400)

