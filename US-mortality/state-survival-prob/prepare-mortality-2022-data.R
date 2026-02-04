# Carl Schmertmann
# 04 Feb 2026
#.....................................................
# Original state-level files from USStateLifetables2022.zip 
# at https://doi.org/10.7910/DVN/19WYUX 
# 
# Original international files from Human Mortality
# Database www.mortality.org
#
# This program must be in the working directory,
# which contains the /Nations and /States subdirectories
# extracted from the USMDB .zip file,
# and the intl-2022.csv file from the HMD
# ....................................................

library('tidyverse')

# read the USFDB state-level life tables ----

state_abb = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT","DC", 
              "DE", "FL", "GA", 
              "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", 
              "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", 
              "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", 
              "UT", "VT", "VA", "WA", "WV", "WI", "WY")


  data = tibble()
  
  for (this_abb in state_abb) {
    
    
   this_filename = paste0('./States/',this_abb,
                          '/',this_abb,"_",
                          'bltper_1x1.csv')
   
   print(this_filename)
   
   tmp = read_csv(this_filename, show_col_types = FALSE) %>%
          mutate( Age = as.numeric(Age)) %>% 
          filter(Year == 2022, Age <= 100) %>% 
          select(pop=PopName,age=Age,lx) 
   
    data = bind_rows(data, tmp)
   
  }
  
# add the equivalent calculations for the entire USA in 2022 ----
  
  this_filename = './Nationals/USA/USA_bltper_1x1.csv'

  print(this_filename)
  
  tmp = read_csv(this_filename, show_col_types = FALSE) %>%
    mutate( Age = as.numeric(Age)) %>% 
    filter(Year == 2022, Age <= 100) %>% 
    select(pop=PopName,age=Age,lx)
  
  data = bind_rows(data, tmp)
  
# add the international HMD data (downloaded separately) ----
  
  tmp = read_csv('intl-2022.csv', skip=5)
  
  data = bind_rows(data, tmp)
  
# save the results to mortality-2022.csv
  
  write_csv(data, file='mortality-2022.csv')

  
 
