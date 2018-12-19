library(tidyverse)
library(scales)

# abbreviations
tmp = data.frame( name=state.name, abb = state.abb) 
tmp = rbind(tmp,  data.frame(name=c('Puerto Rico','District of Columbia'), abb=c('PR','DC')))

D = read.csv('nst-est2018-popchg2010_2018.csv', as.is=TRUE) %>%
      filter(SUMLEV==40) %>%
      select(name    = NAME, 
             pop2010 = ESTIMATESBASE2010, 
             pop2018 = POPESTIMATE2018) %>%
      mutate( gain= factor(pop2018 > pop2010)) %>%
      left_join( tmp) 


# reorder name 
o = order(D$pop2010)
D$name = factor( D$name, levels=D$name[o])

#------------------------------------
# FIRST PLOT:  2010 POP and change
#------------------------------------

ggplot( data=D, aes(x=pop2010, y=name, label=abb, color=gain)) +
   geom_point(size=2, alpha=.80, color='darkgrey') +
   geom_text(size=3, nudge_x= -1e6, fontface='bold') +
   geom_segment(aes(x=D$pop2010, xend=D$pop2018,  
                    y=D$name, yend=D$name), lwd=1,
                arrow=arrow(angle=25,type="open",ends="last",length = unit(0.15, "cm"))) +
   theme_bw() +
   theme(axis.text.y = element_blank(), 
         axis.title.y = element_blank(), 
         axis.ticks.y = element_blank()) +
   labs(x='Resident Population', 
        title='2010-2018 Population Change by State',
        caption='Source: https://www2.census.gov/programs-surveys/popest/datasets/2010-2018/national/totals/nst-est2018-popchg2010_2018.csv') +
   guides(color=FALSE)   +
   scale_color_manual(values=c('red','black')) 

ggsave(filename='pop and change.png', width=11, height=8.5, units='in')

#------------------------------------
# 2nd PLOT:  change
#------------------------------------

D = D %>%
     mutate( change = pop2018 - pop2010)

# reorder for prettier plot
D$name <- factor(D$name, levels = D$name[order(D$change)])

ggplot( data=D, aes(x=change, y=reorder(name,change), label=abb, color=gain)) +
  geom_point(aes(x=0, y=name),size=2, alpha=.80, color='darkgrey') +
  geom_text(size=3, , nudge_x = 
              ifelse(D$gain=='TRUE', 6e4, -6e4)) +
  geom_segment(aes(x=0, xend=change,  
                   y=D$name, yend=D$name), lwd=1,
               arrow=arrow(angle=25,type="open",ends="last",length = unit(0.15, "cm"))) +
  theme_bw() +
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        axis.ticks.y = element_blank()) +
  labs(x='Change in Population', 
       title='2010-2018 Population Change by State',
       caption='Source: https://www2.census.gov/programs-surveys/popest/datasets/2010-2018/national/totals/nst-est2018-popchg2010_2018.csv') +
  guides(color=FALSE)   +
  scale_color_manual(values=c('red','black')) +
  scale_x_continuous(labels=comma)

ggsave(filename='just change.png', width=11, height=8.5, units='in')

