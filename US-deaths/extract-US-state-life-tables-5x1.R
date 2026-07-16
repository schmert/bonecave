library('here')
library('tidyverse')

zipfile = 'USStateLifetables2022.zip'

abbs = sort( c(state.abb, 'DC'))


for (sex in c('b','m','f')) {

  df = tibble()
  
  for (this_abb in abbs) {
   
    this_file  = paste0(this_abb,'_',sex,'ltper_5x1.csv')
    this_fpath = paste0('States/',this_abb,'/',this_file)
    
    unzip(zipfile, files=this_fpath, junkpaths = TRUE)
    this_data = read_csv(this_file, show_col_types = FALSE) |> 
                 select(Year,PopName,Age,mx:ex) |> 
                 add_column(sex,.after='PopName')
    
    df = bind_rows(df, this_data)
    
    file.remove(this_file)
    print(this_abb)
  }
  
  outfile = paste0('US-state-',sex,'ltper_5x1.csv')
  write_csv(df, outfile)
  
  print(outfile)
}

