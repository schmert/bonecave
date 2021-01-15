# Demographic composition of US congressional districts
# Carl Schmertmann
# 17 Dec 2020

library(tidyverse)
library(tidycensus)

# get API key
my_api_key = scan('C:/Users/Carl/Dropbox/my-API-key.txt', what='character')

census_api_key(my_api_key)  # get your key at http://api.census.gov/data/key_signup.html

data(fips_codes)

fips = fips_codes %>% 
           group_by(state) %>% 
           slice(1) %>% 
           select(state, state_name, state_code) %>% 
           mutate(state_num = as.numeric(state_code))

vars = c('Total'='B03002_001',
         'WNH'  ='B03002_003',
         'BNH'  ='B03002_004',
         'INH'  ='B03002_005',
         'ANH'  ='B03002_006',
         'Hisp' ='B03002_012')



D = get_acs(geography = 'congressional district',
            variables = vars) %>% 
            pivot_wider(id_cols     = GEOID,
                        names_from  = variable,
                        values_from = estimate) %>% 
            mutate(state_num = as.numeric(substr(GEOID,1,2)),
                   dist_num  = as.numeric(substr(GEOID,3,4))) %>% 
            left_join(fips) %>% 
            filter(!(state %in% c('DC','PR')), Total >0) %>% 
            mutate(frac_wnh = WNH/Total,
                   frac_bnh = BNH/Total,
                   frac_inh = INH/Total,
                   frac_anh = ANH/Total,
                   frac_hisp= Hisp/Total,
                   district = paste(state,dist_num,sep='-')) %>% 
           select(state,district,contains('frac'))



  
}

