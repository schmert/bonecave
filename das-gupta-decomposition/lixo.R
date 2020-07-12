library(tidyverse)

# read Das Gupta data
# age categories: 
#    1 = 14-24,  2= 25-34, 3= 35-44, 4 = 45+
# region categories
#    1 = urban, 2= rural non-farm, 3= rural farm
# marital categories
#    1 = single, 2 = married spouse present, 3= other
# sex categories
#    1 = male, 2 = female
DG = read_csv(file='das-gupta-1978-example-data.csv')

head(DG)

## crude rates
rate40 = 100 * sum(DG$lf40)/sum(DG$pop40)
rate70 = 100 * sum(DG$lf70)/sum(DG$pop70)

change = rate70 - rate40
change

## one-way decomp by age

tmp =DG %>% 
      group_by(age) %>%
      summarize( pop40 = sum(pop40),
                 lf40  = sum(lf40),
                 pop70 = sum(pop70),
                 lf70  = sum(lf70),
                 rate40 = 100 * lf40/pop40,
                 rate70 = 100 * lf70/pop70) %>% 
      ungroup() %>% 
      mutate( pop40 = prop.table(pop40),
              pop70 = prop.table(pop70),
              avg_rate = (rate40+rate70)/2,
              avg_pop  = (pop40+pop70)/2,
              rate_diff = rate70-rate40,
              pop_diff  = pop70-pop40) %>% 
      select(-contains('lf'))

c('change'=change,
     'rate_effect' = sum(tmp$rate_diff * tmp$avg_pop),
     'comp_effect' = sum(tmp$pop_diff * tmp$avg_rate))


## two-way decomp by age & sex

tmp =DG %>% 
  group_by(age,sex) %>%
  summarize( pop40 = sum(pop40),
             lf40  = sum(lf40),
             pop70 = sum(pop70),
             lf70  = sum(lf70),
             rate40 = 100 * lf40/pop40,
             rate70 = 100 * lf70/pop70) %>% 
  ungroup() %>% 
  mutate( pop40 = prop.table(pop40),
          pop70 = prop.table(pop70),
          avg_rate = (rate40+rate70)/2,
          avg_pop  = (pop40+pop70)/2,
          rate_diff = rate70-rate40,
          pop_diff  = pop70-pop40) %>% 
  select(-contains('lf'))

c('change'=change,
  'rate_effect' = sum(tmp$rate_diff * tmp$avg_pop),
  'comp_effect' = sum(tmp$pop_diff * tmp$avg_rate))



#------------------------------------------------------
#  input arrays
#     E1 and E2 for numerators
#     N1 and N2 for denominators
#
#  have dimensions DIM (eg. DIM=c(4,3,3,2) or DIM=c(3) )
#  the number of dimensions is NDIM = length(DIM)
#
#  rates are 
#     E1/N1 for population 1
#     E2/N2 for population 2
#------------------------------------------------------

# example:  age only

E1 =pull(DG, lf40) %>% array(., c(4,3,3,2))
E2 =pull(DG, lf70) %>% array(., c(4,3,3,2))
N1 =pull(DG, pop40)%>% array(., c(4,3,3,2))
N2 =pull(DG, pop70)%>% array(., c(4,3,3,2))
                             

DIM  = dim(E1)
if (is.null(DIM)) dim=length(E1)

NDIM = length(DIM)

tmp  = expand.grid(sapply(DIM,seq))

for (k in 1:NDIM) {
  
}

## function to write out all the sel+0,sel+1,...sel+(NDIM-1) 
## combinations for a selected index out of a list of NDIM

combination_list = function(sel,n) {
  lapply(0:(n-1), function(k) 
    cbind(sel, combn( seq(n)[-sel],k) %>% t())
   )
}

share = function(indices) {
  
}

