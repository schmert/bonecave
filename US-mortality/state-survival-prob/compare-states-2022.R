# Carl Schmertmann
# 30 Jan 2026
#.....................................................
# Original files from USStateLifetables2022.zip 
# at https://doi.org/10.7910/DVN/19WYUX 
# 
# This program must be in the working directory,
# into which the /Nations and /States subdirectories
# from the .zip file must have already been extracted
# ....................................................

library('tidyverse')

already_processed = file.exists('Qdata.csv')

if (already_processed) {
  my_data = read_csv('Qdata.csv')
} else {

  USMDB = 'USStateLifetables2022.zip'
  
  state_abb = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT","DC", 
                "DE", "FL", "GA", 
                "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", 
                "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", 
                "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", 
                "UT", "VT", "VA", "WA", "WV", "WI", "WY")
  
  my_data = tibble()
  
  for (this_abb in state_abb) {
   this_filename = paste0('./States/',this_abb,
                          '/',this_abb,"_",
                          'bltper_1x1.csv')
   
   tmp = read_csv(this_filename) %>% 
          filter(Year == 2022,
                 Age %in% c(0:40,75:100)) %>% 
          mutate( Age = as.numeric(Age),
                  Q0  = 1 - lx/lx[Age==0],
                  Q75 = if_else(Age >= 75, 1 - lx/lx[Age==75],NA)) %>% 
          select(state=PopName,Age,Q0, Q75)
   
   my_data = bind_rows(my_data, tmp)
   
  }
  
  # add the equivalent calculations for the entire USA in 2022
  this_filename = './Nationals/USA/USA_bltper_1x1.csv'
  
  tmp = read_csv(this_filename) %>% 
    filter(Year == 2022,
           Age %in% c(0:40,75:100)) %>% 
    mutate( Age = as.numeric(Age),
            Q0  = 1 - lx/lx[Age==0],
            Q75 = if_else(Age >= 75, 1 - lx/lx[Age==75],NA)) %>% 
    select(state=PopName,Age,Q0, Q75)
  
  my_data = bind_rows(my_data, tmp)
  
  write_csv(my_data, file='Qdata.csv')

} 

#...............................................

