library(tidyverse)
library(HMDHFDplus)

#getHFDcountries()
#getHFDitemavail(CNTRY='JPN')

# Cohort TFR 13-40 from Lexis triangles ----

BTR = readHFDweb(CNTRY='JPN', item='birthsTR', username=un, password = pw) %>% 
      rename(Births=Total)

NTR = readHFDweb(CNTRY='JPN', item='exposTR', username=un, password = pw)

DTR = full_join(BTR,NTR) 

TFR_triangle = 
      DTR %>% group_by(Cohort,Age) %>% 
             summarize(rate = sum(Births)/sum(Exposure)) %>%
             filter(Age %in% 13:40) %>%
             group_by(Cohort) %>% 
             summarize(TFR=sum(rate), n=n()) %>% 
             filter(n==28) %>% 
             add_column(method='triangle')

# Cohort TFR from Age-Period rectangles ----

BRR = readHFDweb(CNTRY='JPN', item='birthsRR', username=un, password = pw) %>% 
  rename(Births=Total)

NRR = readHFDweb(CNTRY='JPN', item='exposRR', username=un, password = pw)

DRR = full_join(BRR,NRR) %>% 
     mutate(Cohort = Year-Age,
            rate = Births/Exposure)

TFR_rectangle = 
  DRR %>% 
  filter(Age %in% 13:40) %>%
  group_by(Cohort) %>% 
  summarize(TFR=sum(rate), n=n()) %>% 
  filter(n==28) %>% 
  add_column(method='rectangle')

tmp = bind_rows(
        TFR_triangle, 
        TFR_rectangle
        )

G = ggplot(data=tmp) + 
  aes(x=Cohort,y=TFR, shape=method, color=method) + 
  geom_point() + 
  theme_bw() +
  scale_shape_manual(values=c('square','triangle')) +
  labs(title='JPN Cohort Fertility over ages 13-40',
       y='CFR40')

print(G)

# new procedure: average the lagged and leading "rectangle" 
# measures for each cohort

TFR_new = TFR_rectangle %>% 
            ungroup() %>% 
            mutate(nextCohort = lead(Cohort,1),
                   nextTFR    = lead(TFR,1),
                   newTFR     = (TFR+nextTFR)/2)

G + geom_line(data=TFR_new,aes(x=Cohort,y=newTFR), color='black')

@@@NEXT STEP: CAN WE USE THE EXPOSURE RR DATA TO PICK 
WEIGHTS OTHER THAN (0.5,0.5) IN THE NEWTFR FORMULA TO
AMELIORATE THE COHORT SIZE BIAS?