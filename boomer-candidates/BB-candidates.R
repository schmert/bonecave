library(tidyverse)

BByrs = 1946:1964

candidates = tribble(
  ~name, ~yob,
  'Trump', 1946,
  'Bennet', 1964,
  'Biden', 1942,
  'Bloomberg', 1942,
  'Booker', 1969,
#  'Bullock', 1966,
  'Buttigieg', 1982,
  'Castro', 1974,
  'Delaney', 1963,
  'Gabbard', 1981,
  'Harris', 1964,
  'Klobuchar', 1960,
  'Patrick', 1956,
  'Sanders', 1941,
#  'Sestack', 1951,
  'Steyer', 1957,
  'Warren', 1949,
  'Williamson', 1952,
  'Yang', 1975
) %>%
  group_by(yob) %>% 
  summarize(name=paste(name,collapse=' & '))


births = read.table(file='USAtotbirthsRR.txt',skip=2,header=TRUE) %>%
           mutate( boomer = (Year %in% BByrs)) %>%
           left_join(candidates, by=c('Year'='yob'))

theme_carl <- function () { 
  theme_bw(base_size=13) %+replace% 
    theme(
      title      = element_text(size=22, face='bold', hjust =0.5),
      axis.text  = element_text(size=15, face='bold'),
      axis.title = element_text(size=15, face='bold')
    )
}

G = ggplot(data=births) +
      aes(x=Year, y=Total, fill=boomer) +
      geom_bar(stat='identity', alpha=.75) +
      guides(fill=FALSE) +
      scale_x_continuous(breaks=seq(1930,2020,10), minor_breaks = NULL) +
      scale_y_continuous(breaks=seq(0,4e6,2e6), minor_breaks = NULL,
                         labels=c('0','2 mil','4 mil'),
                         limits=c(0, 4.8e6)) +
      scale_fill_manual(values=c('orangered','royalblue')) +
      labs(x='Birth Year',y='Total US Births') +
      theme_carl()

# add candidate names
G = G + 
    geom_segment(data=filter(births, !is.na(name)),
                   aes(x=Year, y=Total+1e4, xend=Year, yend=Total+2e5)) +
    geom_text(aes(x=Year, y=Total+2.5e5, label=name),
              angle=90, hjust=0, size=3.4,vjust=0.3,
              fontface='bold') +
    geom_text(x=1955,y=2e6, label='Boomers', size=12, hjust=0.5,
              color='navy') 

# add ages
ref_ages = seq(80,20,-10)

agelabels = c('Age\n80',paste0('\n',tail(ref_ages,-1)))

G + annotate('text',x=2019-ref_ages, 
                  y=rep(5e5,length(ref_ages)), 
                  label=agelabels, size=5,hjust=0.5, fontface='bold')



ggsave('BB-candidates.png', height=8, width=11, units='in', dpi=300)


