library(tidyverse)
library(rstan)

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
  2016,26,161,
  2018,26,128
)  %>% 
  mutate(p = z/n,
          se = sqrt(p*(1-p)/n))

D


MODEL = stan_model(file = 'gss.stan', model_name = 'GSS')


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
               iter       = 5000,
               warmup     = 1000, 
               thin       = 1,
               chains     = nchains,
)

theta = as.matrix(fit,'theta')

Q = apply(theta,2,quantile,probs=c(.05,.50,.95)) %>% 
      t() %>% 
    as_tibble() %>% 
    add_column(year=1989:2018) %>% 
    rename(q05 = `5%`, q50=`50%`, q95=`95%`)

ggplot(data=Q) +
  geom_line(aes(x=year,y=q50),size=1,color='blue') +
  geom_ribbon(aes(x=year,ymin=q05,ymax=q95),
              fill='steelblue',alpha=.30) +
  geom_point(data=D,aes(x=year,y=p),pch=1) +
  scale_y_continuous(limits=c(0,.22)) +
  scale_x_continuous(breaks=seq(1990,2020,5),
                     minor_breaks = 1990:2020) +
  labs(title='Fraction of Males under 30 with\nno female sex partners since age 18',
       y='Fraction',
       caption='Source: GSS, https://twitter.com/graykimbrough/status/1373430546970382340?s=20') +
  theme_bw() +
  theme(plot.caption = element_text(family='mono',face='bold',size=6))

ggsave(filename='gss.png')