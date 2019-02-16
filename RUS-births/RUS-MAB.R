library(tidyverse)

R = read.table('RUSmabRRbo.txt', skip=2, header=TRUE) %>%
      select(Year,MAB1:MAB4) %>%
      gather(key=order,value=MAB, -Year)

ggplot(R, aes(x=Year, y=MAB, color=order)) +
    geom_line(lwd=2) +
    geom_vline(xintercept=1990) +
    labs(title='Russia: Mean Age at Birth for 1st,2nd,3rd,4th child',
         caption='Source: Human Fertility Database http://humanfertility.org') +
    theme_bw()

ggsave( file='Russia MAB.png', width=8, height=8, units='in',dpi=300)