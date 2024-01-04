library(tidyverse)
library(HMDHFDplus)

# un = your_HMD_username
# pw = your_HMD_password

D = readHMDweb(CNTRY='SWE', item='Deaths_lexis',
               username=un, password = pw) %>% 
        rename(f_deaths= Female,
               m_deaths= Male) %>% 
        select(Year,Age,Cohort,ends_with('_deaths'))

N = readHMDweb(CNTRY='SWE', item='Exposures_lexis',
               username=un, password = pw) %>% 
  rename(f_expos= Female,
         m_expos= Male) %>% 
  select(Year,Age,Cohort,ends_with('_expos'))

df = full_join(D,N,by=c('Year','Age','Cohort')) %>% 
      filter(Age %in% 20:39) %>% 
      group_by(Cohort) %>% 
      summarize( n_lexis_triangles = n(),
                 f_mort_rate = 1000 * sum(f_deaths)/sum(f_expos),
                 m_mort_rate = 1000* sum(m_deaths)/sum(m_expos),
                 ratio = m_mort_rate/f_mort_rate) %>% 
      filter(n_lexis_triangles==40)

ggplot(data=df) +
  aes(x=Cohort) +
  geom_line(aes(y=f_mort_rate), color='red', lwd=1) +
  geom_line(aes(y=m_mort_rate), color='blue', lwd=1) +
  scale_x_continuous(breaks=seq(1725,1985,25),
                     minor_breaks = NULL) +
  theme_bw() +
  theme(text = element_text(face='bold', size=13)) +
  labs(x='Year of Birth',
       title='Sweden: Death Rate per 1000 at Ages 20-39',
       y = 'Deaths per 1000') +
  geom_text(x=1800,y=7,label='Female', color='red', fontface='bold',size=5) +
  geom_text(x=1800,y=13,label='Male', color='blue', fontface='bold',size=5)

ggsave(filename='cohort-rates.png', 
       width=8, height=8, units='in', dpi=300)
  
ggplot(data=df) +
  aes(x=Cohort) +
  geom_line(aes(y=ratio), color='purple', lwd=1) +
  scale_x_continuous(breaks=seq(1725,1985,25),
                     minor_breaks = NULL) +
  theme_bw() +
  theme(text = element_text(face='bold', size=13)) +
  labs(x='Year of Birth',
       title='Sweden: Male/Female Death Rate at Ages 20-39',
       y = 'Male/Female Deaths')

ggsave(filename='cohort-ratios.png', 
       width=8, height=8, units='in', dpi=300)



