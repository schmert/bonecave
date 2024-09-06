###############################################
# Carl Schmertmann
# 06 September 2024
# 
# UN World Population Prospects 2024
# Population of Brazil by age (0-49 or 50+)
# including forecasts with 80% intervals
###############################################

library(wpp2024)
library(tidyverse)
library(here)  # points to bonecave/

rm(list=ls())
graphics.off()

data(popAge1dt)
data(popprojAge1dt)

BRpop = popAge1dt %>% 
         tibble() %>% 
         filter(country_code == 76) %>% 
         select(country_code,name,year,age,pop) %>% 
         mutate(year=as.numeric(year))

BRproj= popprojAge1dt %>% 
  tibble() %>% 
  filter(country_code == 76) %>% 
  select(country_code,name,year,age,pop,pop_low,pop_high) %>% 
  mutate(year=as.numeric(year))

BR = bind_rows(BRpop, BRproj) %>% 
     group_by(country_code,name,year) %>% 
     summarize(jovem = sum(pop[age < 50]),
               velho = sum(pop[age >= 50]),
               jovem_low = sum(pop_low[age < 50]),
               velho_low = sum(pop_low[age >= 50]),
               jovem_high = sum(pop_high[age < 50]),
               velho_high = sum(pop_high[age >= 50]))

  
# 
# create plot

png(filename=here('UN-WPP','old-young-Brazil.png'),
    width=8, height=8, units='in', res=300)

G = ggplot(data=BR) +
  geom_line(aes(x=year,y=jovem), color='blue',lwd=1.5) +
  geom_ribbon(aes(x=year,ymin=jovem_low,ymax=jovem_high),
              color=NA, fill='blue',alpha=0.1) +
  geom_line(aes(x=year,y=velho), color='red',lwd=1.5) +
  geom_ribbon(aes(x=year,ymin=velho_low,ymax=velho_high),
              color=NA, fill='red',alpha=0.1) +
  geom_hline(yintercept = 0, color=NA) +
  geom_vline(xintercept = 2023.5, lty='dotted',lwd=1.5) +
  theme_bw() +
  theme(axis.title = element_text(size=14, face='bold'),
        axis.text = element_text(size=12, face='bold'),
        plot.title = element_text(size=16, face='bold')) +
  scale_x_continuous(breaks=seq(1950,2100,20)) +
  scale_y_continuous(breaks=seq(0,150000,50000),
                     labels=seq(0,150,50)) +
  labs(title='População por faixa etária, Brasil 1949-2100',
       subtitle = 'Dados e previsões da ONU, WPP 2024',
       caption = 'WPP 2024\n@CSchmert',
       x='Ano',y='Milhões de Pessoas') +
  geom_text(x=1970,y=130000, label='Idade 0-49',size=7,color='blue') +
  geom_text(x=1970,y= 22000, label='Idade 50+',size=7,color='red') 

  print(G)

dev.off()
