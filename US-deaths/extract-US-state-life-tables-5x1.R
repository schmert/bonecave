#...........................................................
#  reads the zip file of state-level life tables
#  from the US Mortality Database
#  
#  Barbieri, M., and Winant, C. 
#  “U.S State Life Tables CSV + TXT 1941-2022,” 2025. 
#  https://doi.org/10.7910/DVN/19WYUX
#  
#  and extracts and writes three csv files 
#  (m=male, f=female, b=both sexes combined)
#  containing the state-level 5[age]x1[year] tables
#...........................................................

library('here')
library('tidyverse')


zipfile = 'USStateLifetables2022.zip'

abbs = sort( c(state.abb, 'DC'))


age = c(0,1,seq(from=5,to=110,by=5))

for (sex in c('b','m','f')) {

  df = tibble()
  
  for (this_abb in abbs) {
   
    this_file  = paste0(this_abb,'_',sex,'ltper_5x1.csv')
    this_fpath = paste0('States/',this_abb,'/',this_file)
    
    unzip(zipfile, files=this_fpath, junkpaths = TRUE)
    this_data = read_csv(this_file, show_col_types = FALSE) |> 
                 mutate(Sex=sex) |> 
                 group_by(PopName,Year,Sex) |> 
                 mutate(Age=age) |> 
                 ungroup() |> 
                 select(PopName,Year,Sex,Age,mx:ex) 
    
    df = bind_rows(df, this_data)
    
    file.remove(this_file)
    print(this_abb)
  }
  
  outfile = paste0('US-state-',sex,'ltper_5x1.csv')
  write_csv(df, outfile)
  
  print(outfile)
}

