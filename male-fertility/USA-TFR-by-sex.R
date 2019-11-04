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
       
tmp = big %>%
           group_by(Sex,Year) %>%
           summarize(TFR=sum(ASFR))

theme_carl <- function () { 
  theme_bw(base_size=13) %+replace% 
    theme(
      title      = element_text(size=20, face='bold'),
      plot.caption  = element_text(size=10, face='italic',hjust=0),
      axis.text  = element_text(size=15, face='bold'),
      axis.title = element_text(size=15, face='bold')
    )
}

tmp = big %>%
  filter(Year > 1974) %>%
  group_by(Sex,Year) %>%
  summarize(TFR=sum(ASFR))

G = ggplot(data=tmp) +
      aes(x=Year, y=TFR, group=Sex, color=Sex) +
      geom_line(lwd=1) +
      geom_point() +
      geom_point(x=1988, y=1.925, shape=1, size=9, color='purple') +
      geom_hline(yintercept = 2, lty='dotted') +
      scale_color_manual(values=c('red','blue')) +
      scale_x_continuous(breaks=seq(1975,2015,5)) +
      labs(x='Year',
           y='Total Fertility Rate',
           title='Period Total Fertility 1975-2015\nUSA Males and Females',
           caption='Sources\nMALES: C Dudel and S Klüsener, Human Fertilty Collection https://www.fertilitydata.org/data/RAW_DATA/m_USA_51.zip\nFEMALES: Human Fertility Database') +
      theme_carl()


print(G)

ggsave(G, file='USA-TFR-by-sex.png', height=8, width=11, units='in', dpi=300)

