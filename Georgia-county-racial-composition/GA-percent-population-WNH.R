# GA county populations - racial composition
# Carl Schmertmann
# 5 Jan 2021

library(tidyverse)
library(tidycensus)
library(sf)


data(fips_codes)
  
fips = fips_codes %>%   
      filter(state == 'GA') %>% 
      mutate(GEOID = as.numeric(paste0(as.numeric(state_code), county_code)))

# get API key
my_api_key = scan('C:/Users/Carl/Dropbox/my-API-key.txt', what='character')

census_api_key(my_api_key)  # get your key at http://api.census.gov/data/key_signup.html



vars = c('Total' = 'B03002_001',
         'WNH'   = 'B03002_003',
         'BNH'   = 'B03002_004',
         'Hisp'  = 'B03002_012')

D =  get_acs(geography = 'county', state='GA',
            year = 2019,
            variables = vars,
            geometry = FALSE) %>% 
            mutate(GEOID = as.numeric(GEOID)) %>% 
            pivot_wider(id_cols     = GEOID,
                        names_from  = variable,
                        values_from = estimate) %>% 
            mutate(frac_WNH  = WNH/Total,
                   frac_BNH  = BNH/Total,
                   frac_Hisp = Hisp/Total)


vars = c('Total' = 'B03002_001',
         'WNH'   = 'B03002_003',
         'BNH'   = 'B03002_004',
         'Hisp'  = 'B03002_012')

geo =  get_acs(geography = 'county', state='GA',
             year = 2019,
             variables = c('Total' = 'B03002_001'),
             geometry = TRUE) %>% 
             select(GEOID,geometry) %>% 
             mutate(GEOID=as.numeric(GEOID))

GA = left_join(geo,D)


ggplot(data=GA) +
  geom_sf(aes(fill=frac_WNH), color=NA) +
  scale_fill_viridis_c() +
  theme_minimal() +
  theme(legend.position = 'left')


