library(tidyverse)
library(HMDHFDplus)

US = readHMD('cMx_1x1.txt') %>%
      select(Year,Age,mx=Total) %>%
      filter(Age < 41) 

surv40 = US %>%
          group_by(Year) %>%
          mutate(Hx = cumsum(c(0,head(mx,-1))),
                 surv = exp(-Hx)) %>%
          filter(Age==40)

          


ggplot(data=US) +
  aes(x=Year, y=Total/1e6) +
  geom_point() +
  geom_line() +
  theme_bw() +
  labs(title='Annual US births',
       y='Millions of Births',
       caption='Source: Human Fertility Database\n@CSchmert') +
  scale_y_continuous(limits = c(0,max(US$Total/1e6)))

  