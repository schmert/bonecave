library(tidyverse)
library(HMDHFDplus)

graphics.off()
rm(list=ls() )

# e0 SWE males Lexis triangles ----
DTR = readHMD('./Data/SWE_Deaths_lexis.txt') %>% 
          select(Year,Age,Cohort,Deaths=Male) %>% 
          filter(Cohort > 1849)
              
NTR = readHMD('./Data/SWE_Exposures_lexis.txt') %>% 
          select(Year,Age,Cohort,Exposure=Male) 

TR = full_join(DTR,NTR) %>% 
  mutate(rate = Deaths/Exposure) %>% 
  filter(Cohort > 1849, Age < 100) %>% 
  group_by(Cohort, Age) %>% 
  summarize(Deaths=sum(Deaths),
            Exposure = sum(Exposure),
            rate = Deaths/Exposure) %>% 
  add_column(method='TR')


# Age-Period rectangles ----

DRR = readHMD('./Data/SWE_Deaths_1x1.txt') %>% 
  select(Year,Age,Deaths=Male) 

NRR = readHMD('./Data/SWE_Exposures_1x1.txt') %>% 
  select(Year,Age,Exposure=Male) 


RR = full_join(DRR,NRR) %>% 
     mutate(Cohort = Year-Age,
            rate = Deaths/Exposure) %>% 
     filter(Cohort > 1849, Age < 100) %>% 
     add_column(method='RR')

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

plot(tmp,pch=22, cex=.80,
     main='SWE Males: AP diagonal estimates of e0')

# Lexis triangle estimates

z = TR %>% 
  group_by(Cohort) %>% 
  summarize(e0 = calc_e0(rate))

points(z,pch=2, cex=.80,col='red')
lines(tmp,col='black')

complete_cohort = nobs_df$Cohort[nobs_df$n == 100]
  
df = rbind(TR,RR) %>% 
      filter(Cohort %in% complete_cohort) %>% 
      group_by(method,Cohort) %>% 
      summarize(e0 = calc_e0(rate))

G = ggplot(data=df) +
  aes(x=Cohort,y=e0,color=method, shape=method) +
  geom_point() +
  theme_bw() +
  scale_shape_manual(values=c('square','triangle'))

print(G)

z = df %>%
     ungroup() %>% 
     filter(method=='RR') %>% 
     arrange(Cohort) %>% 
     mutate( e0_new = (e0 + lead(e0,1))/2)

G + geom_line(data=z, aes(x=Cohort,y=e0_new),color='black')