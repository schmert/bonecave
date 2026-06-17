library('tidyverse')
library('HMDHFDplus')

# create a list of countries with cohort fertility tables

clist = getHFDcountries()$CNTRY

df = tibble()

for (this_country in clist) {  
  print(this_country)
  ii = getHFDitemavail(CNTRY=this_country)$item
  avail = 'cft' %in% ii
  if (avail) {
    
    tmp1 = readHFDweb(CNTRY=this_country, item='cft', username=un, password=pw) %>% 
      select(Cohort, Age=x, starts_with('l')) %>% 
      add_column(CNTRY=this_country, .before=1) %>%
      filter(Age %in% c(30,35,40,45,50)) %>% 
      mutate(Age = as.numeric(Age)) 
    
    tmp2 = readHFDweb(CNTRY=this_country, item='ccfrVH', username=un, password=pw) %>% 
      select(Cohort, Age, CCFR) %>% 
      add_column(CNTRY=this_country, .before=1) 
    
    tmp = left_join(tmp1,tmp2, by=join_by(CNTRY,Cohort,Age))
    
    
    df = bind_rows(df, tmp)      
  }
}


info = tribble(
  ~par, ~var,
  '0 children' ,'l0x',
  '1 child'    ,'l1x',
  '2 children' ,'l2x',
  '3+ children','l3x'
)

x = df %>% 
  filter(Age==40, Cohort==1984) %>% 
  mutate(l3x = l3x+l4x) %>% 
  select(-l4x) %>% 
  pivot_longer(cols=starts_with('l'),
               names_to = 'var',
               values_to = 'value') %>% 
  left_join(info) %>% 
  mutate(pct = value/100) 

ggplot(data=x) +
  aes(x=CCFR, y=pct, label=CNTRY) +
  geom_text(size=2) +
  theme_bw() +
  scale_x_continuous(limits=c(1,2.2),
                     breaks=seq(1,2,.50)) +
  scale_y_continuous(limits=c(0,max(x$pct))) +
  labs(title='Fertility and Parity of Women Born in 1984, at age 40',
       x='Avg Number of Children/Woman [CCFR40]',
       y='Percent',
       caption='Source: Cohort Fertility tables at humanfertility.org\nCalculations:@Cschmert') +
  facet_grid(~par) +
  geom_label(data=filter(x,CNTRY=='PRT'),
             color='darkred',size=2,
             fontface='bold')

ggsave(filename='hfd-cohort-fertility-tables.png')
