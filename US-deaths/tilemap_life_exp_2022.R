library('here')
library('tidyverse')
library('geofacet')

zipfile = 'USStateLifetables2022.zip'


abbs = sort( c(state.abb, 'DC'))

df = tibble()

for (this_abb in abbs) {
  this_file  = paste0(this_abb,'_bltper_5x1.csv')
  this_fpath = paste0('States/',this_abb,'/',this_file)
  
  unzip(zipfile, files=this_fpath, junkpaths = TRUE)
  this_data = read_csv(this_file, show_col_types = FALSE) |> 
               filter(Year==2022, Age=='65-69') |> 
               select(Year,PopName,Age,ex) |> 
               mutate(life_exp = factor(floor(ex)))
  df = bind_rows(df, this_data)
  
  file.remove(this_file)
  print(this_abb)
}

df = df |> 
      mutate(life_exp = factor(life_exp,levels=sort(unique(as.character(life_exp))))) |> 
      left_join(us_state_grid1, by= join_by(PopName==code)) |> 
      mutate( x = col,
              y = max(row)-row+1)

for (L in LETTERS[1:8]) {
  G = ggplot(data=df) +
    aes(x, y, fill = life_exp, label=PopName) +
    geom_tile(color='white') +
    geom_text(color='white',size=3,fontface = 'bold') +
    theme_minimal() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border     = element_blank(),
          panel.background = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank()) +
    scale_fill_viridis_d(direction = -1, option=L ) +
    labs(title=L)
  
  print(G)
}
