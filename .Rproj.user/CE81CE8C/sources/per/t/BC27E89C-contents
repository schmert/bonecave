library(tidyverse)
library(HMDHFDplus)

graphics.off()
rm(list=ls() )

source('fix-HFDparse.R')  # this is necessary for some versions of HFDparse (?)

# Cohort TFR 13-40 from Lexis triangles ----

BTR = readHFD('JPNbirthsTR.txt')  %>% 
  rename(Births=Total)

NTR = readHFD('JPNexposTR.txt')

DTR = full_join(BTR,NTR) 

TFR_triangle = 
      DTR %>% group_by(Cohort,Age) %>% 
             summarize(rate = sum(Births)/sum(Exposure)) %>%
             filter(Age %in% 13:40) %>%
             group_by(Cohort) %>% 
             summarize(TFR=sum(rate), n=n()) %>% 
             filter(n==28) %>% 
             add_column(method='True CFR')


# Cohort TFR from Age-Period rectangles ----

BRR = readHFD('JPNbirthsRR.txt') %>% 
  rename(Births=Total)

NRR = readHFD('JPNexposRR.txt')

DRR = full_join(BRR,NRR) %>% 
     mutate(Cohort = Year-Age,
            rate = Births/Exposure)

TFR_rectangle = 
  DRR %>% 
  filter(Age %in% 13:40) %>%
  group_by(Cohort) %>% 
  summarize(TFR=sum(rate), n=n()) %>% 
  filter(n==28) %>% 
  add_column(method='AP')

# new procedure: average the lagged and leading "rectangle" 
# measures for each cohort

TFR_new = TFR_rectangle %>% 
  ungroup() %>% 
  mutate(TFR     = (TFR+ lead(TFR,1))/2,
         method='AP2')


df = bind_rows(
        TFR_triangle, 
        TFR_rectangle,
        TFR_new
        )

G = ggplot(data=df) + 
  aes(x=Cohort,y=TFR, shape=method, color=method) + 
  geom_point(size=2, alpha=.70) + 
  geom_line(data=filter(df,method=='AP2')) +
  theme_bw() +
  scale_shape_manual(values=c('square','cross','triangle')) +
  labs(title='JPN Cohort Fertility over ages 13-40',
       y='CFR40',
       subtitle='Line = Avg of consecutive A-P squares')

print(G)

ggsave(filename='JPN-CFR-levels.png',
       height=8, width=8, units='in')

# calculate errors

# df = df %>% 
#   mutate(method=case_match(method,
#                            'Lexis Triangles'~'Tri',
#                            'A-P Squares'~'Squ',
#                            'Avg A-P Squares'~'New'))

err_df = df %>% 
  group_by(Cohort) %>% 
  summarize(e_old = 100*(TFR[method=='AP']/TFR[method=='True CFR']-1),
            e_new = 100*(TFR[method=='AP2']/TFR[method=='True CFR']-1)) %>% 
  pivot_longer(cols=starts_with('e_'), 
               names_to = 'etype', values_to = 'error')


G = ggplot(data=err_df) +
  aes(x=Cohort,y=error,color=etype) +
  geom_hline(yintercept = 0) +
  geom_point() +
  geom_line() +
  theme_bw() +
  labs(y='Percent Error')

print(G)

ggsave(filename='JPN-CFR-errors.png',       
       height=8, width=8, units='in')


  
