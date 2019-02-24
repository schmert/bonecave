###########################################
# Brazilian metropolotian regions 
# 2010 census populations
###########################################

rm(list=ls())
library(tidyverse)

graphics.off()
windows(record=TRUE)

df = read.csv('Brazilian-metro-populations-2010.csv',
              fileEncoding = 'UTF-8') %>%
             mutate(Mil = (pop2010>1e6),
                    seq = seq(metro_code))

ggplot(data=df, aes(x=pop2010, y=reorder(metro_area,pop2010),
                    color=Mil)) +
           geom_point(size=3) +
           scale_x_continuous(breaks=c(0,1e6,5e6,10e6,15e6,20e6),
                              minor_breaks = NULL,
                              labels=c('0','1 Mil','5 Mil',
                                       '10 Mil','15 Mil','20 Mil')) +
           geom_vline(xintercept=1e6) +
           scale_color_discrete(guide=FALSE) +
           labs(title='Population of Brazilian Metropolitan Regions, 2010',
                subtitle = '(21 regions have more than 1 million residents)',
                caption = 'Source: Instituto Brasileiro de Geografia e Estatística (IBGE), 2010 Census',
                x = 'Resident Population 2010',
                y = '') +
           theme_bw()

ggsave(file='Brazilian-metro-populations-2010.png',
       width=11, height=8.5, units='in', dpi=300)

if (FALSE) {

    big = df %>%
            filter(Mil) %>%
            mutate(ii = 1 + (seq-1) %% 5,
                   jj = 4 - floor((seq-1)/5),
                   metro_label = paste0(metro_area,'\n',
                                        sprintf("%.1f", 
                                          pop2010/1e6),' Mil'))
    
    # small adjustment to make spacing better
    big$jj[big$jj==4] = 4.2
    
    ggplot( data=big, aes(x=ii, y=jj, size=pop2010, label=metro_label )) +
      scale_size(range=c(2,40), guide=FALSE) +
      geom_point(color='orangered',alpha=.80) +
      scale_y_continuous(breaks=NULL,minor_breaks = NULL,
                         limits=c(0,5)) +
      scale_x_continuous(breaks=NULL, minor_breaks = NULL,
                         limits=c(0,6)) +
      geom_text(size=2.5, fontface='bold') +
      labs(x='',y='',
           title  ='Relative Populations of Brazilian Metro Regions, 2010',
           caption='Source: Instituto Brasileiro de Geografia e Estatística (IBGE), 2010 Census') +
      theme_minimal()
       
    ggsave(file='Brazilian-metro-populations-2010-as-areas.png',
           width=11, height=8.5, units='in', dpi=300)
}