library('tidyverse')
library('HMDHFDplus')

intl = tibble()

info = tribble(
  ~abb, ~name,
  'FRATNP', 'France',
  'SWE', 'Sweden',
  'ESP','Spain',
  'GBR_NP','UK'
)

for (i in 1:nrow(info)) {
  this_CNTRY = info$abb[i]
  this_name  = info$name[i]
  
tmp = readHMDweb(CNTRY=this_CNTRY, item='bltper_1x1',
               username=un, password = pw) %>% 
      filter(Year == 2022, Age %in% 0:100) %>% 
      mutate(pop=this_name) %>% 
      select(pop,age=Age,lx)

intl = bind_rows(intl, tmp)
}


