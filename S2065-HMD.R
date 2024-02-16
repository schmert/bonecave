#...................................................................
# Carl Schmertmann
# 16 Feb 2024
# 
# Comparative time trends: (period) prob of male surviving 
# from age 20 to age 65 in USA, Sweden, Japan, and France
# ..................................................................

library(tidyverse)
library(HMDHFDplus)


# reading HMD data requires a username and password for the 
# site.  These have to be already stored in variables called
# "un" and "pw", respectively. 

# read period life tables for males

countries = c('USA','FRATNP','SWE','JPN')

df = tibble()

for (this_country in countries) {

  tmp = readHMDweb(CNTRY=this_country, item='mltper_1x1',
                       username=un, password=pw) %>% 
              filter(Year %in% 1970:2019) %>% 
              group_by(Country=this_country,Year) %>% 
              summarize(S = lx[Age==65] / lx[Age==20]) 
  
  df = bind_rows(df, tmp)
  
}



# plot the survival probabilities

txt_df = tribble(
  ~Country, ~PrettyCountry, ~Year, ~S,
  'USA'   , 'USA'   , 2016, .80, 
  'FRATNP', 'France', 2016, .84,
  'SWE'   , 'Sweden', 2016, .91,
  'JPN'   , 'Japan' , 2016, .89
)

png(file='S2065-HMD.png', height=8, width=8, units='in', res=300)

ggplot(data=df) +
  aes(x=Year, y=S, color=Country) +
  geom_point(shape=1,size=2) +
  geom_line(lwd=1) +
  geom_text(data=txt_df, aes(label=PrettyCountry), fontface='bold') +
  scale_x_continuous(breaks=seq(1950,2020,10)) +
  theme_bw() +
  guides(color='none') +
  labs(title='Period Probability of Male Survival from Age 20 to Age 65',
       y='Survival Probability',
       caption='Source: Human Mortality Database https://mortality.org\n@CSchmert')
  

dev.off()

