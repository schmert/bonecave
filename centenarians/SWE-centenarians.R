library(tidyverse)

D = read.table('SWE_cMx_1x1.txt', skip=2, header=TRUE) %>%
       select(Year:Male) %>% 
       gather(-Year, -Age, key='sex', value='mx') %>%
       group_by(Year,sex) %>%
       mutate(Hx = head(cumsum(c(0,mx)),-1),
              lx = exp(-Hx))

cent = D %>%
        filter(Year > 1849, Age==100, is.finite(lx)) %>%
        mutate(p100 = 100*lx)

ggplot(data=cent, aes(x=Year, y=p100, color=sex)) +
         geom_line(lwd=3, alpha=.80) +
         labs(title='Probability of Reaching 100th birthday\nSweden Males and Females',
              x = 'Year of Birth',
              y = 'Percent of Birth Cohort',
              caption = 'Source: Human Mortality Database [ http://mortality.org ]') +
         theme_bw() +
         theme( axis.text = element_text(size=11, face='bold'),
                axis.title =  element_text(size=11, face='bold'))

ggsave(file='SWE-cent-zoom-in.png', height=8, width=8, units='in', dpi=300)  
        
ggplot(data=cent, aes(x=Year, y=p100, color=sex)) +
  geom_line(lwd=3, alpha=.80) +
  labs(title='Probability of Reaching 100th birthday\nSweden Males and Females',
       x = 'Year of Birth',
       y = 'Percent of Birth Cohort',
       caption = 'Source: Human Mortality Database [ http://mortality.org ]') +
  theme_bw() +
  theme( axis.text = element_text(size=11, face='bold'),
         axis.title =  element_text(size=11, face='bold')) +
  scale_y_continuous(limits=c(0,50)) +
  scale_x_continuous(limits=c(1850,2020)) +
  geom_hline(yintercept = 50, lty=2, color='black') +
  geom_vline(xintercept = 2019, lty=2, color='black') +
  geom_text(x=2013, y= 45, label='?', face='bold', size=10, inherit.aes = FALSE)

ggsave(file='SWE-cent-zoom-out.png', height=8, width=8, units='in', dpi=300)  


       
