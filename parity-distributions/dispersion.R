rm(list=ls())

library(tidyverse)
library(readr)

ipums = read_csv(file='ipums-data.csv', skip=4)

summary_stat = ipums %>%
                group_by(country,year) %>%
                summarize( mean = weighted.mean(chborn,n),
                           meansq = weighted.mean(chborn^2,n),
                           var = meansq - mean^2,
                           disp = var/mean) %>%
                select(-meansq)

summary_stat

D = ipums %>%
      left_join(summary_stat) %>%
      group_by(country,year) %>%
      mutate( poisson_chborn = sum(n) * dpois(chborn, lambda=mean),
              ti = paste(country[1],year[1],'Mean=',round(mean,2),'Var=',round(var,2),
                         '\nVar/Mean=',round(disp,2),
                         '\nBars= Observed Parity, Dots= Poisson'))

D

ggplot(data= D,
       aes(x=chborn, y=n)) +
      scale_x_continuous(breaks=0:16) +
      geom_bar(stat='identity', fill='royalblue', alpha=.60) +
      geom_point(data=D, aes(x=chborn, y=poisson_chborn),
                 size=4) +
     geom_line(data=D, aes(x=chborn, y=poisson_chborn) ) +
     theme_bw() +
     theme(strip.text = element_text(size=12, face='bold')) +
     facet_wrap(~ti)

ggsave('ipums-dispersion.png', height = 8.5, width=11, units='in', dpi=300)
