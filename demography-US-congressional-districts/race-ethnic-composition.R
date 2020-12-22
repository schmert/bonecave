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

# create a new "state" variable that splits CA,FL,NY,TX into two parts
# each

D = D %>% 
     arrange(state,frac_wnh) %>% 
     group_by(state) %>% 
     mutate(ndist = n(),
            i = 1:ndist) %>% 
     ungroup() 

D$st = D$state
ch1 = (D$state %in% c('CA','FL','NY','TX')) & (D$i <= D$ndist/2)
ch2 = (D$state %in% c('CA','FL','NY','TX')) & (D$i  > D$ndist/2)
D$st[ ch1 ] = paste0(D$st[ch1],':1') 
D$st[ ch2 ] = paste0(D$st[ch2],':2') 


ggplot(data=D) +
  geom_point(aes(x=frac_wnh, y=reorder(district,frac_wnh)),
             shape='circle',size=1, alpha=.50, color='blue') +
  geom_point(aes(x=frac_bnh, y=reorder(district,frac_wnh)),
             shape='diamond',size=1, alpha=.50, color='red') +
  geom_point(aes(x=frac_inh, y=reorder(district,frac_wnh)),
             shape='square',size=1, alpha=.50, color='darkgreen') +
  geom_point(aes(x=frac_anh, y=reorder(district,frac_wnh)),
             shape=12,size=1, alpha=.50, color='orange') +
  geom_point(aes(x=frac_hisp, y=reorder(district,frac_wnh)),
             shape='triangle',size=1, alpha=.50, color='purple') +
  theme_bw() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_x_continuous(limits=c(.05,1),
                     breaks=seq(0,1,.50)) +
  geom_vline(xintercept=c(.25,.50,.75,1), lty='dotted') +
  labs(x='',y='') +
  facet_wrap(~st, scales='free_y') 

################################


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

