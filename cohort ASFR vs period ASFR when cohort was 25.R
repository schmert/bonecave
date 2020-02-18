library(HMDHFDplus)
library(tidyverse)
library(showtext)

font_add_google('Source Sans Pro', 'source_sans')

showtext_auto()

theme_carl <- function () { 
  theme_bw(base_size=13, base_family="source_sans") %+replace% 
    theme(
      axis.text        = element_text(size=50, face='bold'),
      legend.text      = element_text(size=40),
      legend.title     = element_text(size=40),
      panel.grid.major = element_line(color=grey(.80),size=0.3),
      panel.grid.minor = element_line(color=grey(.95),size=0.1),
      strip.text       = element_text(size=13, face='bold'),
      plot.title       = element_text(size=70,hjust=0),
      plot.subtitle    = element_text(size=45,hjust=0),
      axis.title       = element_text(size=60)
    )
}


this_country = 'USA'


# cohort x year
VV = readHFDweb(CNTRY=this_country, item='asfrVV',
                 username=un, password = pw) 

# age x year
RR = readHFDweb(CNTRY=this_country, item='asfrRR',
                username=un, password = pw) 

sel_cohort = 1959

fcoh = VV %>%
        filter(Cohort == sel_cohort,
               ARDY %in% 15:49) %>%
        pull(ASFR)

fper = RR %>%
        filter(Year == sel_cohort + 25,
               Age %in% 15:49) %>%
         pull(ASFR)

df = tibble( age  = 15:49, fcoh, fper) %>%
      gather(key='type', value='asfr', -age) 

df$age[df$type=='fcoh'] = df$age[df$type=='fcoh'] - .50


graphics.off()

png(file=paste0(this_country,
                '-cohort ASFR vs period ASFR when cohort was 25.png'), 
    height=8, width=8, 
    units='in', res=300)

ggplot(data=df) +
    aes(x=age, y=asfr, group=type, color=type) +
    geom_line(size=2) +
    labs(title=paste(this_country,'HFD data'),
         subtitle=paste(sel_cohort,'Cohort vs.', 
                        sel_cohort+25, 'Period Rates'),
         x='Age',
         y='Fertility Rate') +
    geom_vline(xintercept=25) +
    theme_carl()

dev.off()
