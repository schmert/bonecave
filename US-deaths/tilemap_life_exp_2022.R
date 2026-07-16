library('here')
library('tidyverse')
library('geofacet')

rm(list=ls())

this_sex = 'm'
this_sexname = c('m'='Male','f'='Female','b' = 'Both Sexes')[this_sex]

this_age = 50
this_year = 2022

abbs = sort( c(state.abb, 'DC'))

df = tibble()

for (this_abb in abbs) {
  this_file  = paste0('US-state-',this_sex,'ltper_5x1.csv')
  
  this_data = read_csv(this_file, show_col_types = FALSE) |> 
               filter(Year==this_year, Age==this_age) |> 
               select(Year,PopName,Age,ex) |> 
               mutate(life_exp = factor(trunc(ex)))
  
  df = bind_rows(df, this_data)
  
  print(this_abb)
}

exvals = df$life_exp |> 
          as.character() |> 
          unique() |> 
          as.numeric() |> 
          sort()

exlabs = paste(exvals,exvals+1, sep='-')


hues = c('#fef0d9',
  '#fdd49e',
  '#fdbb84',  '#fc8d59',
  '#ef6548',  '#d7301f',
  '#990000')

hues = c('#8c510a',
'#d8b365',
'#f6e8c3',
'#f5f5f5',
'#c7eae5',
'#5ab4ac',
'#01665e')

hues = c('#d73027',
'#fc8d59',
'#fee08b',
'#ffffbf',
'#d9ef8b',
'#91cf60',
'#1a9850')

  df = df |> 
      mutate(life_exp = factor(life_exp,levels=exvals,labels=exlabs)) |> 
      left_join(us_state_grid1, by= join_by(PopName==code)) |> 
      mutate( x = col,
              y = max(row)-row+1)

  G = ggplot(data=df) +
    aes(x, y, fill = life_exp, label=PopName) +
    geom_tile(color='white') +
    geom_text(color='black',size=2.5,fontface = 'bold') +
    theme_minimal() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border     = element_blank(),
          panel.background = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          plot.title = element_text(hjust=0.5)) +
#    scale_fill_viridis_d(direction = -1, option='D') +
scale_fill_manual(values=hues) +
        labs(title=paste(this_year,': Remaining',this_sexname,
                     'Life Expectancy\nat Age',this_age),
         fill='Years')
  
  print(G)
