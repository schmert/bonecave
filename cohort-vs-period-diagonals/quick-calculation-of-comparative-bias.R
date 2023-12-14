library(tidyverse)

df = expand_grid( K=c(.90,1,1.1), delta=c(-.20,0,.20)) %>% 
      mutate(Fhat_bias = -delta/(1+K),
             Ftil_bias = (1-K)/2 * Fhat_bias)

df %>% round(4)