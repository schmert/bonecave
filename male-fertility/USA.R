# USA male fertility
# from Dudel, C. and S. Klüsener. Male fertility data for high-income 
# countries [unpublished data]. Submitted to the HFC by C. Dudel 
# on 15.05.2019.
# 
# downloaded from Human Fertility Collection 2 Nov 2019
# https://www.fertilitydata.org/data/RAW_DATA/m_USA_51.zip

library(tidyverse)

# Male data from HFC, as described above
MM = read.table(file='m_ASFR_USA.txt', header=TRUE) %>%
      mutate(Y = factor(Year)) 

# Female data from HFD
FF = read.table(file='USAasfrTR.txt', skip=2, header=TRUE) %>%
       filter(Year %in% unique(MM$Year)) %>%
       mutate(Age = 11 + as.numeric(Age)) %>%
       group_by(Year,Age) %>%
       summarize(ASFR = mean(ASFR)) %>%
       mutate(Y=factor(Year))

big = bind_rows(
         mutate(MM,Sex='Male'),
         mutate(FF,Sex='Female')
       )
       
moments = big %>%
           group_by(Sex,Year) %>%
           summarize(mean = weighted.mean(Age+.5, ASFR),
                     expsq = weighted.mean((Age+.5)^2, ASFR),
                     sd = sqrt(expsq - mean^2)) %>%
           select(Sex,Year,mean,sd)

theme_carl <- function () { 
  theme_bw(base_size=13) %+replace% 
    theme(
      title      = element_text(size=20, face='bold'),
      plot.caption  = element_text(size=12, face='italic'),
      axis.text  = element_text(size=15, face='bold'),
      axis.title = element_text(size=15, face='bold')
    )
}


G = ggplot(data=moments) +
      aes(x=sd, y=mean, group=Sex, color=Sex) +
      geom_path(lwd=1) +
      scale_color_manual(values=c('red','blue')) +
      labs(x='Std Dev',
           y='Mean',
           title='Mean and SD of Age of Childbearing\nUSA Males and Females',
           caption='Source: C Dudel and S. Klüsener, Human Fertilty Collection\nhttps://www.fertilitydata.org/data/RAW_DATA/m_USA_51.zip') +
      theme_carl()


zz = moments %>% 
      filter(Year %% 10 == 0 | Year == max(moments$Year))

G = G + geom_point(data=zz,  size=2) +
      geom_text(data=zz, label=zz$Year,nudge_x = .1) 

print(G)

ggsave(G, file='USA.png', height=8, width=8, units='in', dpi=300)

