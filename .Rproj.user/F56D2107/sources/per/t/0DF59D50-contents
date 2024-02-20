library(tidyverse)
library(here)

# data from the SDA analysis system on ipums.org 
data = read_csv(here('abroad.csv'), skip=8) %>% 
         select(-1)

# drop the last row with totals, and overwrite the age column with 0-97
df = head(data, -1) %>% 
        mutate(age = 0:97) %>% 
        rename('NativeBorn' = 2, 'ForeignBorn'=3, 'Total'=4)

# condense the 90+ group

df = df %>% 
      mutate( agecat = cut(age, breaks=c(0:90,Inf), right=FALSE) ) %>% 
      group_by(agecat) %>% 
      summarize(NB = sum(NativeBorn),
                FB = sum(ForeignBorn),
                Total = sum(Total)) %>% 
      mutate(age = seq(agecat)-1)

G  = ggplot(df) +
  aes(x=age, y=100*FB/Total) +
  geom_point(color='blue', size=2) +
  theme_bw() +
  labs(title='Percent of US Residents Born Abroad, by Age, in 2022',
       y='Percent Born Abroad',
       x='Age',
       caption='US Census Bureau, via ipums.org\n2022 American Community Survey\n@CSchmert') +
  scale_y_continuous(limits=c(0,30)) +
  scale_x_continuous(breaks=seq(0,90,10))

ggsave(here('Percent-US-residents-born-abroad.png'))