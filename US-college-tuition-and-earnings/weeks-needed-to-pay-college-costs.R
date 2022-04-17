

library(tidyverse)

# 
# Bureau of Labor Statistics
# Consumer Price Index
# Series Id:    CUUR0000SA0
# 
# Not Seasonally Adjusted
# Series Title: All items in U.S. city average, all urban consumers, not seasonally adjusted
# Area:         U.S. city average
# Item:         All items
# Base Period:  1982-84=100
# 

CPI_all = tibble(
  year = 1969:2021,
  cpi = c(36.7,38.8,40.5,41.8,44.4,49.3,
          53.8,56.9,60.6,65.2,72.6,82.4,
          90.9,96.5,99.6,103.9,107.6,109.6,
          113.6,118.3,124.0,130.7,136.2,
          140.3,144.5,148.2,152.4,156.9,
          160.5,163.0,166.6,172.2,177.1,
          179.9,184.0,188.9,195.3,201.6,
          207.342,215.303,214.537,218.056,
          224.939,229.594,232.957,236.736,
          237.017,240.007,245.120,251.107,
          255.657,258.811,270.970))  %>% 
  mutate(multiplier = cpi/cpi[year==2019])

#"Table 330.10. Average undergraduate tuition, fees, room, and board rates charged for full-time students in degree-granting postsecondary institutions, by level and control of 
#              institution: Selected years, 1963-64 through 2019-20"																								
# https://nces.ed.gov/programs/digest/d20/tables/dt20_330.10.asp
# "Total tuition, fees, room and board for 4-year institutions
# Constant 2019-2020 dollars
# year in this df is first half of academic year:
# e.g. "1968" means 1968-1969 AY

# convert NCES costs (which are in 2019-2020 dollars) into the
# equivalent nominal amounts, using overall CPI

costs = tibble(year = 1968:2019,
      real_costs = c(         
        11144,11402,11554,11737,12202,11570,10860,10922,11294,11191,
        10955,10494,10391,10799,11547,11997,12551,13011,13792,13928,
        14275,14609,14601,15332,15806,16354,16637,17199,17548,17935,
        18585,18764,18985,19689,20395,21433,22154,22559,23277,23529,
        24400,25073,25682,26010,26541,27041,27615,28211,28189,28360,
        28121,28775)) %>% 
       full_join(CPI_all) %>% 
       mutate(nominal_costs = real_costs * multiplier)


# Bureau of Labor Statistics
# https://data.bls.gov/pdq/SurveyOutputServlet
# Median wkly earnings, Emp FT, Wage & sal wrkrs - LEU0252881500 

earnings = tibble(
   year = 1979:2021,
   nominal_earnings = c(
          241,262,284,302,313,326,344,359,374,385,399,
          412,426,440,459,467,479,490,503,523,549,576,
          596,608,620,638,651,671,695,722,739,747,756,
          768,776,791,809,832,860,886,917,984,998)
)

data = full_join(costs,earnings) %>% 
        filter(year %in% 1979:2019) %>% 
        mutate(weeks_needed = nominal_costs/nominal_earnings)

text_df = filter(data, year %in% c(1979,2019)) %>% 
           mutate(txt = paste0(sprintf('%2.1f', weeks_needed),'\nwks')) %>% 
           select(year,weeks_needed, txt)

ggplot(data=data) +
  aes(x=year, y=weeks_needed) +
  geom_line(color='darkgreen', size=1.5) +
  theme_bw(base_size=14) +
  theme(axis.text = element_text(face='bold'),
        axis.title = element_text(face='bold')) +
  scale_y_continuous( limits=c(0,35)) +
  labs(title='US Four-Year College Costs\n(Tuition, Fees, Room, and Board)',
       x='Year',y='Weeks of Work',
       caption='Source: Natl Center for Education Statistics\nBureau of Labor Stati\n@cschmert') +
  geom_label(data=text_df, aes(label=txt), face='bold', 
             nudge_y = -3, size=4, color='darkgreen') +
  geom_text(x=1990, y=14, hjust=0, 
            label=paste0('Weeks of Work Required',
                         '\nat Median Weekly Earnings',
                         '\nto pay 1 year\'s college costs'), color='darkgreen')


ggsave('weeks-needed-to-pay-college-costs.png', height=6, width=6, dpi=300)

