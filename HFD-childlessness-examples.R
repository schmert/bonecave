#......................................................
# means and std deviations of ASFR curves from HFD
#......................................................

library(tidyverse)
library(HMDHFDplus)
library(here)

# your HFD username and password must already 
# be available in objects called un and pw, 
# respectively

countries = getHFDcountries()
codes     = countries$CNTRY

df = tibble()

for (this_code in c('USA','JPN','CZE')) {
  
  try({
    
  tmp = readHFDweb(CNTRY=this_code, item='tfrVHbo', 
                   username=un, password = pw) %>% 
         add_column(country=this_code, .before=1) %>% 
         select(country,cohort=Cohort,CCF40_1) %>% 
         mutate(childless = 100*(1-CCF40_1))
  
   df = df %>% bind_rows(tmp)
   
  }) # try
}

txt_df = df %>% 
          group_by(country) %>% 
          slice_max(cohort,n=1)

G = ggplot(data=df) +
  aes(x=cohort, y=childless, color=country) +
  geom_point() +
  geom_smooth(span=.35,se=FALSE) +
  theme_bw() +
  labs(x='Year of Birth\n(Current Age)',y='% Childless at Age 40',
       title="Percent of Women Childless at Age 40",
       subtitle='USA, Japan, and Czechia',
       caption='Human Fertility Database https://humanfertility.org\n@CSchmert') +
  scale_y_continuous(limits=c(0,32)) +
  scale_x_continuous(limits=c(1935,1985),
                     breaks=seq(1940,1980,10),
                     labels=c('1940\n(84)',
                              '1950\n(74)',
                              '1960\n(64)',
                              '1970\n(54)',
                              '1980\n(44)')) +
  geom_text(data=txt_df, aes(label=country), hjust=0, nudge_x = 1) +
  guides(color='none')
 
print(G)

  ggsave(filename=here('HFD-childless-examples.png'), plot=G,
         height=8, width=6, units='in',dpi=300)
  
