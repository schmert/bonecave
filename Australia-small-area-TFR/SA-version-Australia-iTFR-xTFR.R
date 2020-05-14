library(tidyverse)
library(sf)
library(rgdal)

AUS = readOGR('SA2 ERP by Age and Sex GeoPackage 2018.gpkg') %>%
       st_as_sf() %>%
       mutate_at(vars(M0_4:P85_and_over), 
                 function(x) {as.numeric(as.character(x))}) %>% 
       mutate( C = M0_4 + F0_4,
               W15 = F15_19,
               W20 = F20_24,
               W25 = F25_29,
               W30 = F30_34,
               W35 = F35_39,
               W40 = F40_44,
               W45 = F45_49,
               W = W15+W20+W25+W30+W35+W40+W45,
               p2534 = (W25+W30)/W,
               CWR = C/W,
               iTFR = 7*CWR,
               xTFR = (10.65-12.55*p2534)*CWR,
               xTFRcat = cut(xTFR,
                             breaks=c(-Inf,seq(1.4,2.3,.30),Inf),right=TRUE)) %>%
       select(contains('STATE_'), contains('SA'),
              C:xTFRcat)


G = ggplot(data=AUS) + 
      geom_sf(aes(fill=xTFRcat),color='lightgrey',alpha=.60) + 
      geom_sf(aes(fill=xTFRcat),color=NA) + 
      scale_x_continuous(limits=c(110,160)) +
      scale_fill_viridis_d(option='C', direction=-1) +
      labs(title='Total Fertility Rate (xTFR) 2018',
           caption='Source: ABS https://tinyurl.com/y9b6schd') +
      theme_minimal() +
      scale_fill_manual(values=c('red','pink','gold','lightblue','blue')) +
  
ggsave(G, filename='SA-version-AUS-xTFR-2018.png',
       width=10, height=8, dpi=300)

# Sydney area
G2 = ggplot(data=AUS) + 
      geom_sf(aes(fill=xTFRcat),color='lightgrey',alpha=.60) + 
#      scale_fill_viridis_d(option='C', direction=-1) +
      labs(title='Total Fertility Rate (xTFR) 2018',
           caption='Source: ABS https://tinyurl.com/y9b6schd') +
      theme_minimal() +
      scale_fill_manual(values=c('red','pink','gold','lightblue','blue')) +
      scale_x_continuous(limits=c(150.1,151.6)) +
      scale_y_continuous(limits=c(-34.3,-33.0)) 

ggsave(G2,filename='Sydney-SA-version-AUS-xTFR-2018.png',
       width=10, height=8, dpi=300)

AUS %>% 
  st_drop_geometry() %>% 
  select(contains('name'), C, W, p2534, xTFR) %>% 
  arrange(-xTFR) %>% 
  head(20) %>%
  mutate_at(vars(p2534,xTFR), round, 2)
