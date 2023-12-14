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
     filter(Cohort > 1799)

