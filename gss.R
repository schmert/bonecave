library(tidyverse)
#library(rstan)

# GSS data on male virginity
# n = #male respondents under 30 with valid data
# z = #reporting zero female partners after age 18
D = tribble(
  ~year, ~z,~n,
  1989,10,149,
  1990,6,114,
  1991,13,121,
  1993,16,128,
  1994,21,214,
  1996,20,254,
  1998,29,213,
  2000,24,221,
  2002,21,222,
  2004,24,221,
  2006,17,198,
  2008,9,148,
  2010,12,143,
  2012,17,147,
  2014,24,179,
  2016,16,161,
  2018,26,128
)  %>% 
  mutate(p = z/n,
          se = sqrt(p*(1-p)))

D

# ggplot(data=D) +
#   aes(x=year,y=p) +
#   geom_point() +
#   geom_line() +
#   theme_bw() +
#   geom_ribbon(aes(x=year, ymin = p-se, ymax=p+se), 
#               fill='grey', alpha=.30)
# 


MODEL = rstan::stan_model(file = 'gss.stan', model_name = 'GSS')


stanInits = function(nchains=1) {
  L = vector('list',nchains)
  
  for (i in seq(L)) {
    L[[i]] =   list(
    
      theta = runif(30,.01,.40),
      sigma = abs(rnorm(1,0,.05))
    )
  }
  return(L)
} # stanInits

stanDataList = list(
  nyrs = nrow(D),
  year = D$year,
  z    = D$z,
  n    = D$n
)

# MCMC 
nchains = 4

## STAN SAMPLING    
fit = sampling(MODEL, 
               data       = stanDataList,
               pars       = c('theta','sigma'),
               init       = stanInits(nchains),
               seed       = 6447100,
               iter       = 200,
               warmup     = 100, 
               thin       = 1,
               chains     = nchains,
)

