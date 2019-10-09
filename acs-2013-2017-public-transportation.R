library(tidyverse)

D = read_csv('acs-2013-2017-public-transportation.csv') %>%
       filter(age < 70) %>%
       mutate(region = factor(div %/% 10,
                              labels = c('Northeast','Midwest','South','West'))) %>%
       group_by(region,age) %>% 
       summarize(pct = 100 * sum(yes)/sum(yes,no))

ggplot(data=D) +
  aes(x=age,y=pct, color=region) +
  geom_point(size=3) +
  geom_line(lwd=1.5) +
  theme_bw() +
  scale_y_continuous(breaks=seq(0,25,5), limits=c(0,25), expand=c(0,.05,.05,.05)) +
  labs(x='Age of Worker',
       y='% Public Transp',
       title='Percent of Workers 16+ traveling to work by Public Transportation',
       caption='Source: US Census Bureau 2013-2017 American Community Survey',
       color='Region')


ggsave(file='acs-2013-2017-public-transportation.png',
       width=8, height=8, units = 'in', dpi=300)
