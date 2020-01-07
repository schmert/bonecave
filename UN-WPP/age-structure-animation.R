###############################################
# observed and forecast youth and old-age fractions
# from UN WPP 2019 data
###############################################

library(wpp2019)
library(tidyverse)

data(pop)
data(popproj)

sel_countries = c('Germany', 
                  'Italy',
                  'United Kingdom',
                  'Sweden',
                  'Nigeria',
                  'Brazil')

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
                     young = sum(pop[x < 20])/total,
                     elder = sum(pop[x > 55])/total)


ggplot(data=big_obs) +
  aes(x=elder, y=young, color=name,group=name) +
  geom_point(size=3) +
  geom_line(size=0.2) +
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
            young = sum(pop[x < 20])/total,
            elder = sum(pop[x > 55])/total)


ggplot(data=big_pred) +
  aes(x=elder, y=young, color=name,group=name) +
  geom_point(size=3) +
  geom_line(size=0.2) +
  theme_bw()


ggplot(data=bind_rows(big_obs,big_pred)) +
  aes(x=elder, y=young, color=name,group=name) +
  geom_point(size=3) +
  geom_line(size=0.2) +
  theme_bw()

