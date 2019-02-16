library(tidyverse)
graphics.off()
windows(record=TRUE)

# Jan 1999 - Dec 2017
D = D[1:228,] %>%
     mutate(month=1:228,
            M   = rep(1:12,19),
            Jan = (M ==1))


G = ggplot(data=D, aes(x=month,y=Deaths)) +
  geom_point(size=.5, color='royalblue') +
  geom_line(color='royalblue') +
  theme_bw() +
  labs(x="Time",title='Monthly Deaths in the United States 1999-2017',
       caption='Source: CDC Wonder https://wonder.cdc.gov') +
  scale_x_continuous(breaks=seq(1,228,24),
                     minor_breaks = NULL,
                     labels=seq(1999,2017,2))

G = G + 
     geom_point(data=filter(D,Jan), pch=16, color='royalblue',size=2) +
     geom_text(data=filter(D,Jan),
               aes(x=month,y=Deaths+4000),
               label='Jan', color='royalblue',
               inherit.aes=FALSE)

     G


ggsave( file='Monthly US deaths 1999-2017.png',
        width=11, height=8.5, units='in', dpi=300)


###########################
LOM = data.frame(
  M = 1:12,
  ndays = c(31,28.25,31,30,31,
            30,31,31,30,31,30,31)
)

agg = D %>%
  group_by(M) %>%
  summarize(Deaths=sum(Deaths)) %>%
  left_join(LOM) %>%
  mutate( DailyDeaths = (Deaths/ndays)/19)



ggplot(data=agg, aes(x=M, y=DailyDeaths)) +
  geom_point(color='darkred',size=2) +
  geom_line(color='royalblue',lwd=2, alpha=.80) +
  theme_bw() + 
  scale_x_continuous(breaks=1:12,
                     minor_breaks = NULL,
                     labels=c('Jan','Feb','Mar',
                              'Apr','May','Jun',
                              'Jul','Aug','Sep',
                              'Oct','Nov','Dec')) +
  labs(x="Month",title='Average Daily Deaths in the United States by Month 1999-2017',
       caption='Source:  CDC Wonder https://wonder.cdc.gov') 


ggsave( file='Average Daily US deaths by Month 1999-2017.png',
        width=11, height=8.5, units='in', dpi=300)

