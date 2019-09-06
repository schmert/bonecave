# Mortality by month and state, Germany 2017
# data from regionalstatistik.de
# https://tinyurl.com/y6hgzcxx

library(tidyverse)


DE = read.csv2(file='12613-03-01-4-B.csv', skip=8, header=FALSE) %>%
       head(16)

names(DE) = c('year','statenum','statename','total','Jan','Feb','Mar','Apr','May',
              'Jun','Jul','Aug','Sep','Oct','Nov','Dec')

days_in_month = c(31,28,31,30,31,30,31,31,30,31,30,31)

daily_avg_deaths = sum(as.matrix(DE[,-(1:4)])) / 365

# matrix of deaths/day by state

daily = sweep( x=as.matrix(DE[,-(1:4)]),
               MARGIN=2,
               STATS = days_in_month,
               FUN = '/')

#-------------------------
# national plot
#-------------------------

df = tibble( month          = factor(colnames(daily),
                                     levels=colnames(daily)), 
             deaths_per_day = colSums(daily),
             region         = 'Germany')


ggplot(data=df, aes(x=month, y=deaths_per_day, group=region)) +
  geom_line(color='blue', lwd=0.8) +
  geom_point(color='blue', size=4, shape=1) +
  geom_hline(yintercept = daily_avg_deaths, lty=2, color='blue') +
  labs(title='Seasonality of Deaths, Germany 2017',
       x='Month',y='Deaths/Day',
       caption='Source: regionalstatistik.de https://tinyurl.com/y6hgzcxx') +
  theme_bw() +
  geom_text(x=1.5, y=25+daily_avg_deaths, label='Avg Day',
            color='blue') +
  theme(plot.title = element_text(hjust=0.5),
        axis.title = element_text(face='bold',size=13),
        axis.text  = element_text(face='bold',size=12))

ggsave(file='seasonality-of-deaths-germany-2017.png',
       width=11, height=8.5, units='in', dpi=300)





