library(tidyverse)

year = function(qtr) {
  tmp = strsplit(qtr,':') %>% 
          unlist() %>%
          matrix(nrow=2)
  yy = as.numeric(tmp[1,])
  rr = case_when(
        tmp[2,] == 'Q1' ~ .125,
        tmp[2,] == 'Q2' ~ .375,
        tmp[2,] == 'Q3' ~ .625,
        tmp[2,] == 'Q4' ~ .875
  )
  return(yy + rr)
}

W = read_csv('US-wealth-by-generation.csv') %>%
      mutate(date = year(Date))

names(W) = str_replace_all(names(W),' ','_')

pdf(file='US-wealth-by-generation.pdf')            

for (this_var in names(W)[3:14]) {
  G = ggplot(data=W) +
        aes_string(x='date', y=this_var, group='Category', color='Category') +
        geom_line(lwd=2) +
        theme_bw() +
        labs(title=this_var)
  
  print(G)
}

dev.off()

