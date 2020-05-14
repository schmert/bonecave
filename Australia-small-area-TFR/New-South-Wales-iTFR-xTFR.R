library(tidyverse)
library(sf)
library(rgdal)

NSW = readOGR('LGA ERP by Age and Sex GeoPackage 2018.gpkg') %>%
       st_as_sf() %>%
       filter(State_name_2016 == 'New South Wales') %>% 
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
               xTFR = (10.65-12.55*p2534)*CWR) %>%
       select(contains('State_'), contains('LGA_'),
              C:xTFR)


ggplot(data=NSW) + 
  geom_sf(aes(fill=xTFR),color=NA) + 
  scale_fill_viridis_c(option='C', direction=-1) +
  scale_x_continuous(limits=c(150.8,151.4)) +
  scale_y_continuous(limits=c(-34.1,-33.7)) +
  labs(title='Total Fertility Rate (SxTFR) 2018',
       caption='Source: ABS https://tinyurl.com/yd5a7tck') +
  theme_minimal()
 
ggsave(filename='NSW-xTFR-2018.png',
       width=10, height=8, dSpi=300)

NSW %>% 
  st_drop_geometry() %>% 
  select(contains('name'), C, W, p2534, xTFR) %>% 
  arrange(-xTFR) %>% 
  head(20) %>%
  mutate_at(vars(p2534,xTFR), round, 2)
