# Broadband access by population density and race
# Carl Schmertmann
# 24 Dec 2020

library(tidyverse)
library(tidycensus)


data(fips_codes) %>% 
  
fips = fips_codes %>%   
      mutate(GEOID = as.numeric(paste0(as.numeric(state_code), county_code)))

density_info = read_csv('Average_Household_Size_and_Population_Density-County.csv',
                        skip=2) %>% 
               rename(density=B01001_calc_PopDensity) %>% 
               select(GEOID,NAME,State,density) %>% 
               left_join(fips, by='GEOID')


# get API key
my_api_key = scan('C:/Users/Carl/Dropbox/my-API-key.txt', what='character')

census_api_key(my_api_key)  # get your key at http://api.census.gov/data/key_signup.html



vars = c('Total'   ='B28008_001',
         'TotalBB' ='B28008_004',
         'White'   ='B28009A_001',
         'WhiteBB' ='B28009A_004',
         'Black'   ='B28009B_001',
         'BlackBB' ='B28009B_004')
         

D = get_acs(geography = 'county',
            year = 2019,
            variables = vars) %>% 
            mutate(GEOID = as.numeric(GEOID)) %>% 
            pivot_wider(id_cols     = GEOID,
                        names_from  = variable,
                        values_from = estimate) %>% 
            left_join(density_info, by='GEOID') %>% 
            filter(!(state %in% c('DC','PR')), Total >0) %>% 
            mutate(frac_total = TotalBB/Total,
                   frac_white = WhiteBB/White,
                   frac_black = BlackBB/Black)


ggplot(data=D) +
  geom_point(aes(x=density,y=frac_total)) +
  geom_smooth(aes(x=density,y=frac_total)) +
  scale_x_log10() +
  theme_bw() 

ggplot(data=D) +
  geom_point(aes(x=density,y=frac_white)) +
  geom_smooth(aes(x=density,y=frac_white)) +
  scale_x_log10() +
  theme_bw() 

ggplot(data=D) +
  geom_point(aes(x=density,y=frac_black)) +
  geom_smooth(aes(x=density,y=frac_black)) +
  scale_x_log10() +
  theme_bw() 

ggplot(data=D) +
  geom_point(aes(x=density,y=frac_white-frac_black)) +
  geom_smooth(aes(x=density,y=frac_white-frac_black)) +
  scale_x_log10() +
  theme_bw() 


# devtools::install_github("ricardo-bion/ggradar",
library(ggradar)

for (sel_state in unique(D$state)) {

  full_state_name = fips %>% 
                     filter(state==sel_state) %>% 
                     pull(state_name)
  nreps = D %>% 
    filter(state==sel_state) %>% 
    slice(1) %>% 
    pull(ndist)
  
  this_title = paste0(full_state_name,' (',nreps,')')
  
  print(this_title)
  
  X = D %>% 
     filter(state==sel_state) %>% 
     select(district,contains('frac')) %>% 
     rename('White\nNon-Hispanic'           = frac_wnh,
            'Black\nNon-Hispanic'           = frac_bnh,
            'Asian\nNon-Hispanic'           = frac_anh,
            'Native American\nNon-Hispanic' = frac_inh,
             Hispanic                       = frac_hisp) 

  G= ggradar(X, base.size=13,
             plot.legend = FALSE, 
             group.line.width = 0.4, 
             group.point.size = 2,
             background.circle.colour = 'white',
             background.circle.transparency = 0,
             axis.label.size=3.5,
             gridline.label.offset = +0.15,
             grid.label.size = 5,
             plot.title = this_title)
  
  # print(G)
  
  ggsave(filename=paste0(sel_state,'-districts.jpg'),
         plot=G,
         dpi=200, width=5,height=5)
  
}

