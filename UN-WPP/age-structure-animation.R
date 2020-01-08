###############################################
# observed and forecast youth and old-age fractions
# from UN WPP 2019 data
###############################################

library(wpp2019)
library(tidyverse)

data(pop)
data(popproj)

yage = 20
oage = 60
xlab = paste0('Fraction ',oage,"+")
ylab = paste0('Fraction Under ',yage)


sel_countries = c('Asia',
                  'Northern America',
                  'Europe', 
                  'South America',
                  'Africa')

# observed populations up through 2020 ----

FF = popF %>%
       select(-country_code) %>%
       filter(name %in% sel_countries) %>% 
       gather(key='period', value='Fpop', -name, -age)

MM = popM %>%
  select(-country_code) %>%
  filter(name %in% sel_countries) %>% 
  gather(key='period', value='Mpop', -name, -age)

big_obs = full_join(FF,MM) %>%
           mutate(pop = Fpop + Mpop) %>%
           transform(x = seq(0,100,5)) %>%
           group_by(name,period) %>%
           summarize(total = sum(pop), 
                     young = sum(pop[x <  yage])/total,
                     elder = sum(pop[x >= oage])/total)


ggplot(data=big_obs) +
  aes(x=elder, y=young, color=name,group=name) +
  geom_point(aes(size=total), alpha=.50) +
  geom_line(size=0.2) +
  labs(x=xlab,y=ylab, 
       title='Age Structure',
       caption='Source: UN World Population Prospects 2019') +
  theme_bw()


# forecasts through 2100 ----

FF = popFprojMed %>%
  select(-country_code) %>%
  filter(name %in% sel_countries) %>% 
  gather(key='period', value='Fpop', -name, -age)

MM = popMprojMed %>%
  select(-country_code) %>%
  filter(name %in% sel_countries) %>% 
  gather(key='period', value='Mpop', -name, -age)

big_pred = full_join(FF,MM) %>%
  mutate(pop = Fpop + Mpop) %>%
  transform(x = seq(0,100,5)) %>%
  group_by(name,period) %>%
  summarize(total = sum(pop), 
            young = sum(pop[x <  yage])/total,
            elder = sum(pop[x >= oage])/total)


ggplot(data=big_pred) +
  aes(x=elder, y=young, color=name,group=name) +
  geom_point(aes(size=total), alpha=.50) +
  geom_line(size=0.2) +
  labs(x=xlab,y=ylab, 
       title='Age Structure',
       caption='Source: UN World Population Prospects 2019') +
  theme_bw()


ggplot(data=bind_rows(big_obs,big_pred)) +
  aes(x=elder, y=young, color=name,group=name) +
  geom_point(aes(size=total), alpha=.50) +
  scale_size_continuous(range=c(0,15)) +
  labs(x=xlab,y=ylab, 
       title='Age Structure',
       caption='Source: UN World Population Prospects 2019') +
  geom_line(size=0.2) +
  theme_bw()

