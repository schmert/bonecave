
library(tidyverse)

# data from https://data.worldbank.org/indicator/SP.DYN.LE00.IN?locations=DK-US

D = read_csv('donny-deutsch-data.csv', skip=2) 

theme_carl <- function () { 
  theme_bw(base_size=13) %+replace% 
    theme(
      title      = element_text(size=22, face='bold', hjust =0.5),
      axis.text  = element_text(size=15, face='bold'),
      axis.title = element_text(size=15, face='bold')
    )
}

ggplot(data=D) +
  aes(x=year, y=e0, color=country) +
  geom_line(lwd=2) +
  scale_color_manual(values=c('red','blue')) +
  scale_x_continuous(breaks=seq(1965,2015,10)) +
  guides(color=FALSE) +
  geom_text(x=1965,y=74.5,label='Denmark', color='red', size=10) +
  geom_text(x=1980,y=71.5,label='USA', color='blue', size=10) +
  labs(title='Life Expectancy at Birth\n(both sexes combined)',
       caption='Source: Human Mortality Database http://mortality.org') +
  geom_vline(xintercept=c(1957,1989,2010),lty='dotted',lwd=1.5) +
  geom_text(x=1957.5,y=79,label='Donny born\n1957',hjust=0, size=4,color='black') +
  geom_text(x=1989.5,y=79,label='Donny named chairman\nof his dad\'s firm 1989',hjust=0, size=4,color='black') +
  geom_text(x=2010.5,y=74.5,label='Donny refuses\nto pay\ncommission\non sale of\n$30 mil home\n2010',hjust=0, size=4,color='black') +
  theme_carl()


ggsave('donny-deutsch-denmark.png',
       height=10, width=8, units='in', dpi=300)
