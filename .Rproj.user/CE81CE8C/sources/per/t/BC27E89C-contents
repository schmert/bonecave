library(tidyverse)
library(HMDHFDplus)

graphics.off()
rm(list=ls() )

# e0 SWE males Lexis triangles ----
DTR = readHMD('SWE_Deaths_lexis.txt') %>% 
          select(Year,Age,Cohort,Deaths=Male) %>% 
          filter(Cohort > 1799)
              
NTR = readHMD('SWE_Exposures_lexis.txt') %>% 
          select(Year,Age,Cohort,Exposure=Male) %>% 
          filter(Cohort > 1799)   


TR = full_join(DTR,NTR) 

# Age-Period rectangles ----

DRR = readHMD('SWE_Deaths_1x1.txt') %>% 
  select(Year,Age,Deaths=Male) 

NRR = readHMD('SWE_Exposures_1x1.txt') %>% 
  select(Year,Age,Exposure=Male) 


RR = full_join(DRR,NRR) %>% 
     mutate(Cohort = Year-Age,
            rate = Deaths/Exposure) %>% 
     filter(Cohort > 1799, Age < 100)

nobs_df = RR %>% 
           group_by(Cohort) %>% 
           summarize(n=n())

RR = left_join(RR, nobs_df, by='Cohort') %>% 
       filter(n==100) %>% 
       arrange(Cohort,Age)

# calc_e0 function

calc_e0 = function(mx) {
  m = mx
  names(m) = seq(m)-1
  Hx = cumsum(c(0,m))
  lx = exp(-Hx)
  Lx = (head(lx,-1) + tail(lx,-1)) / 2
  e0 = sum(Lx)
  return(e0)
}

tmp = RR %>% 
       group_by(Cohort) %>% 
       summarize(e0 = calc_e0(rate))

plot(tmp,pch=22, cex=.70,
     main='SWE Males: AP diagonal estimates of e0')


