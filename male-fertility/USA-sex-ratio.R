# USA male fertility analysis
#
# background:  ratio of males/females from HMD over 1969-2015

library(tidyverse)

# Male data from HFC, as described above
pop = read.table(file='Population.txt', 
                 header=TRUE, skip=2,stringsAsFactors = FALSE) %>%
        mutate(Year = as.numeric(Year)) %>%
        filter(Year %in% 1969:2015)

pop$Age = 0:110

pop = pop %>%
        filter(Age %in% 15:59) %>%
        mutate(sex_ratio=round(100*Male/Female,1),
               Y=factor(Year))


theme_carl <- function () { 
  theme_bw(base_size=13) %+replace% 
    theme(
      title      = element_text(size=18, face='bold'),
      plot.caption  = element_text(size=11, face='italic'),
      axis.text  = element_text(size=15, face='bold'),
      axis.title = element_text(size=15, face='bold')
    )
}

ggplot(data= filter(pop, (Year %% 10 == 0 ) )) +
   aes(x=Age, y=sex_ratio, group=Y, color=Y) +
   geom_line(lwd=1.5) +
   geom_hline(yintercept = 100,lty='dotted',lwd=1) +
   labs(title='Males per 100 Females\nat Ages 15-59, USA',
        caption='Source: Human Mortality Database',
        y='Males per 100 Females') +
   theme_carl()

ggsave(file='USA-sex-ratio.png', height=8, width=8,
       units='in',dpi=300)

