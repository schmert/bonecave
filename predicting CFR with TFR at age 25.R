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
      plot.subtitle    = element_text(size=18,hjust=0),
      axis.title       = element_text(size=60)
    )
}


this_country = 'FRATNP'


# cohort x year
VV = readHFDweb(CNTRY=this_country, item='asfrVV',
                 username=un, password = pw) 

# age x year
RR = readHFDweb(CNTRY=this_country, item='asfrRR',
                username=un, password = pw) 

complete_cohorts = VV %>% 
                   group_by(Cohort) %>%
                   summarize(nage = sum(ARDY %in% 15:49)) %>%
                   filter(nage == 35) %>%
                   pull(Cohort)

y25 = tibble(
        Cohort = complete_cohorts,
        Year   = Cohort + 25
      )
               
coh = VV %>%
        filter(Cohort %in% complete_cohorts) %>%
        group_by(Cohort) %>%
        summarize(cfr = sum(ASFR[ARDY %in% 15:49]))

per = RR %>%
        group_by(Year) %>%
        summarize(tfr = sum(ASFR[Age %in% 15:49]))


df = per %>%
        inner_join(y25) %>%
        inner_join(coh)

this_span = range(df$tfr, df$cfr)

graphics.off()

png(file=paste0(this_country,
                '-predicting CFR with TFR at age 25.png'), 
    height=8, width=8, 
    units='in', res=300)

ggplot(data=df) +
    aes(x=tfr, y=cfr, label=Cohort) +
    scale_x_continuous(limits=this_span) +
    scale_y_continuous(limits=this_span) +
    geom_text(size=10) +
    geom_abline(intercept = 0, slope=1) +
    labs(title=paste(this_country,'HFD data'),
         x='TFR when cohort was 25',
         y='Cohort CFR') +
    theme_carl()

dev.off()
