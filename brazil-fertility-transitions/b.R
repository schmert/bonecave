# Data from 
# Mapping the Timing, Pace, and Scale of the Fertility 
# Transition in Brazil
#
# Population and Development Review 36(2):283-307. 2010. 

library(tidyverse)

rm(list=ls())

# matrix Q contains summary information about microregion-level
# fertility transitions, 
load(url('http://schmert.net/BayesLogistic/brazil_logistic_posterior_summary.11-May-10-2200.gz'))

z = Q['mean', grep('start', colnames(Q))]
start_year = 1960 + 10 * z

z = Q['mean', grep('duration', colnames(Q))]
duration = 10 * z

regions = read.csv('http://schmert.net/BayesLogistic/brazil_logistic_data.csv')

df = tibble(
       start = start_year,
       duration = duration,
       name = regions$name
     )

G = ggplot(data=df) +
       aes(x=start, y=duration, label=name) +
       geom_point() +
       theme_bw() +
       scale_x_continuous(limits=c(1935,1995)) +
       labs(title='Fertility Transition: Brazilian Microregions',
            x='Year Transition Started',
            y='Length of Transition (Years)',
            caption='Data Source: http://schmert.net/BayesLogistic')


early.df = df %>%
             top_n(5, -start)

late.df = df %>%
             top_n(5,  start)

G = G +
     geom_point(data=early.df, shape=0, size=4, fill=NA,color='blue') +
     geom_text(data=early.df, color='blue',size=3, nudge_y = 1) +
     geom_point(data=late.df, shape=0, size=2, fill=NA,color='red') +
     geom_text(data=late.df, color='red',size=3, nudge_y = -1)

G

ggsave('b.png', height=8, width=8, units='in', dpi=300)






