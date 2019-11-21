library(tidyverse)
library(tidycensus)


coastal_CA_counties = c(
  'Del Norte','Humboldt','Mendocino','Sonoma','Marin',
  'San Francisco','San Mateo','Santa Cruz','Monterey',
  'San Luis Obispo','Santa Barbara','Ventura',
  'Los Angeles','Orange','San Diego'
)



census_api_key('...')

coast = get_acs(variables = c('Hispanic'='B03001_003'),
                summary_var = c('Total'='B03001_001'),
  geography = 'county', state='CA',county=coastal_CA_counties) %>%
  mutate(frac_hisp = estimate/summary_est,
         county_name = str_replace(NAME,' County, California',''),
         coastal_order = match(county_name, coastal_CA_counties))

ggplot(data=coast) +
    aes(x=coastal_order, y=frac_hisp, label=county_name) +
    geom_bar(stat='identity', fill='royalblue') +
    theme_bw() +
    geom_text(aes(x=coastal_order), y=.01, angle=90, hjust=0,size=6,
              color='white') +
    theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave('CA Coast.png', height=8.5, width=11, units='in',dpi=300)

