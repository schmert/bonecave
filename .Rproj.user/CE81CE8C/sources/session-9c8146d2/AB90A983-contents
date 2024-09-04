###############################################
# Carl Schmertmann
# 29 Aug 2024
# 
# UN World Population Prospects 2024
# Forecast total population for
# India, China, Pakistan
###############################################

library(wpp2024)
library(tidyverse)
library(here)       # points to bonecave/

rm(list=ls())
graphics.off()

data(UNlocations)
data(pop1dt)
data(popproj1dt)

national_codes = UNlocations %>% 
                   as_tibble() %>% 
                   filter(location_type == 4) %>%
                   pull(country_code)

natpop = pop1dt %>%
          tibble() %>% 
          filter(country_code %in% national_codes)


natpopproj = popproj1dt %>% 
  tibble() %>% 
  filter(country_code %in% national_codes) 


# append the projected populations 
# rescale population to billions

popdata = bind_rows(natpop, natpopproj) %>% 
            mutate(year = as.numeric(year)) %>% 
            arrange(country_code, name, year) %>% 
            select(name,country_code,year,pop,Q10=pop_80l,Q90=pop_80u) %>% 
            mutate(pop = pop/1e6, Q10 = Q10/1e6, Q90=Q90/1e6)

# which countries have largest projected populations
# for 2100? A: top 3 are India, China, Pakistan

top3 = popdata %>% 
        filter(year == 2100) %>% 
        slice_max(pop,n=3) %>% 
        pull(country_code)

big = popdata %>% 
       filter(country_code %in% top3)

text_df = tribble(
  ~year,  ~name, ~pop,
  2050, 'Pakistan', 0.15,
  2050, 'India', 1.90,
  2050, 'China',  0.95
)

png(filename=here('UN-WPP','largest-forecast-populations-2100.png'),
    height=8, width=6, units='in', res=300)

G = ggplot(data=big) +
  aes(x=year, y=pop, color=name,fill=name) +
  geom_line(lwd=1.5) +
  geom_ribbon(aes(ymin=Q10, ymax=Q90),alpha=.20,color=NA) +
  theme_bw() +
  labs(x='Year', y='Population (billions)',
       title='Population of India, China, and Pakistan',
       subtitle='UN Median Projected Population w/ 80% uncertainty',
       caption='UN World Population Prospects 2024\n@CSchmert') +
  guides(color='none',fill='none') +
  geom_vline(xintercept = 2023.5, lty='dotted') +
  scale_x_continuous(breaks=seq(1950,2100, 20)) +
  geom_text(data=text_df, aes(label=name),fontface='bold')

print(G)

dev.off()