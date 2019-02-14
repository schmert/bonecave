graphics.off()
windows(record=TRUE)

library(tidyverse)

M = read.csv('USAmonthly.txt') %>%
     filter( Month != 'TOT') %>%
     mutate(Y = Year,
            M = as.numeric(as.character(Month)),
            month = 12*(Y-1931) + M) %>%
     select(Y,M,month,Births)

ggplot(data=M, aes(x=month,y=Births)) +
           geom_point(size=.5, color='firebrick') +
           geom_line(color='firebrick') +
           theme_bw() +
           labs(x="Time",title='Monthly Births in the United States 1931-2016',
                caption='Source: Human Fertility Database http://humanfertility.org') +
           scale_x_continuous(breaks=seq(1,1032,60),
                              minor_breaks = NULL,
                              labels=seq(1931,2016,5))

ggsave( file='Monthly US births 1931-2016.png',
        width=11, height=8.5, units='in', dpi=300)


LOM = data.frame(
       M = 1:12,
       ndays = c(31,28.25,31,30,31,
                 30,31,31,30,31,30,31)
  )

agg = M %>%
       group_by(M) %>%
       summarize(Births=sum(Births)) %>%
       left_join(LOM) %>%
       mutate( DailyBirths = (Births/ndays)/86)



ggplot(data=agg, aes(x=M, y=DailyBirths)) +
    geom_point(color='darkred',size=2) +
    geom_line(color='firebrick',lwd=2, alpha=.80) +
    theme_bw() + 
    scale_x_continuous(breaks=1:12,
                       minor_breaks = NULL,
                       labels=c('Jan','Feb','Mar',
                                'Apr','May','Jun',
                                'Jul','Aug','Sep',
                                'Oct','Nov','Dec')) +
  labs(x="Month",title='Average Daily Births in the United States by Month 1931-2016',
       caption='Source: Human Fertility Database http://humanfertility.org') 
  

ggsave( file='Average Daily US births by Month 1931-2016.png',
        width=11, height=8.5, units='in', dpi=300)

