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

for (this_code in codes) {
  
  try({
    
  tmp = readHFDweb(CNTRY=this_code, item='asfrRR', 
                   username=un, password = pw) %>% 
         add_column(country=this_code) %>% 
         group_by(country,year=Year) %>% 
         summarize( m = weighted.mean(Age+.50,w=ASFR),
                    v = weighted.mean((Age+.50)^2,w=ASFR)-m^2,
                    s = sqrt(v))
  
   df = df %>% bind_rows(tmp)
   
   G = ggplot(data=tmp) +
        aes(x=m, y=s) +
        geom_point() +
        theme_bw() +
        labs(title=this_code)

   print(G)   
  }) # try
}


G = ggplot(data=df) +
  aes(x=m, y=s) +
  geom_point(color='royalblue', alpha=.40) +
  theme_bw() +
  scale_x_continuous(limits=c(24,33), breaks=seq(24,33,3),
                     minor_breaks = NULL) +
  labs(title='Empirical Mean and SD of age at childbearing',
       subtitle='Dots are HFD country-years. Curve is cubic model',
       x='Mean age of childbearing',
       y='SD of age of childbearing')

cubic_model_df = tibble( m=seq(27,33,.10)) %>% 
             mutate( v= 60*m - 855 - m^2,
                     s = sqrt(v))

G = G + geom_line( data=cubic_model_df, lwd=1) 

ggsave(filename=here('mean-and-sd-of-HFD-fertility-schedules.pdf'),
       plot=G, height=5, width=5, units='in', dpi=300)


