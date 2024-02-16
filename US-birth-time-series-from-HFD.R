library(tidyverse)
library(HMDHFDplus)

US = readHFD('USAtotbirthsRR.txt')

ggplot(data=US) +
  aes(x=Year, y=Total/1e6) +
  geom_point() +
  geom_line() +
  theme_bw() +
  labs(title='Annual US births',
       y='Millions of Births',
       caption='Source: Human Fertility Database\n@CSchmert') +
  scale_y_continuous(limits = c(0,max(US$Total/1e6)))

  