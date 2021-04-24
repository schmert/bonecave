library(tidyverse)

D = read_csv('Foreign-born-age-sex-largest-groups-2015-2019-ACS.csv',
             col_names=c('area','age','sex','pop')) 

Dord = D %>% 
        group_by(area) %>% 
        summarize(pop = sum(pop)) %>% 
        arrange(-pop) 
        

for (this_area in Dord$area) {

  png(filename=paste0(this_area,'.png'), res=300,
      height=1000, width=1400)
  
  tmp = filter(D, area == this_area)

  poptotal = sum(tmp$pop)
    
  this_plot =
  ggplot(data=tmp) +
    aes(x=age+2.5,y=pop/1000, color=sex) +
    geom_point(size=2) +
    geom_line(size=0.8) +
    scale_x_continuous(breaks=seq(0,100,10)) +
#    coord_flip() +
#    facet_wrap(~area, scales="free") +
    theme_bw() +
    geom_vline(xintercept = seq(0,100,25), lty='dashed', color=grey(.50)) +
    labs(x='5-Yr Age Group',
         y='Population (Thousands)',
         title=paste('US residents born in', toupper(this_area)),
         subtitle = paste(round(poptotal/1000), 'thousand'),
         caption='Source: US Census Bureau, 2015-2019 ACS (via ipums.org)') +
    scale_color_manual(values=c('red','blue'))
  
  print(this_plot)
  
  dev.off()
}
