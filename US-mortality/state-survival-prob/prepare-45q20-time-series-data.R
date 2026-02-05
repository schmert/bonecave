# Carl Schmertmann
# 05 Feb 2026
#.....................................................
# Original state-level files from USStateLifetables2022.zip 
# at https://doi.org/10.7910/DVN/19WYUX 
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
          mutate( age = as.numeric(Age)) %>% 
          filter(age %in% c(20,65)) %>% 
          rename(pop=PopName,year=Year) %>%
          summarize(Q = 1 - lx[age==65]/lx[age==20],
                    .by=c(pop,year))
   
    data = bind_rows(data, tmp)
   
  }
  
  write_csv(data, file='45q20-time-series-data.csv')

  
