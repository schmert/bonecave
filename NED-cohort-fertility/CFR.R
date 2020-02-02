library(tidyverse)
library(gganimate)

D = read_delim(file='83790NED_TypedDataSet_02022020_092240.csv',
             col_names=TRUE, delim=';')

names(D) = c('id','cohort','cfr','p0','p1','p2','p3plus','mab1')

D$cohort = as.numeric( substr(D$cohort,1,4))

df = D %>%
       select(id, cohort, p0:p3plus) %>%
       gather(key='var',value='value', -id, -cohort) %>%
       filter(cohort <= 1975) 

txt_df = tribble(
  ~var, ~vartext,
  'p0', '0',
  'p1', '1',
  'p2', '2 children',
  'p3plus', '3+',
)

df = left_join(df, txt_df)
       
theme_carl <- function () { 
  theme_bw(base_size=11) %+replace% 
    theme(
      title      = element_text(size=17, face='bold', hjust =0.5),
      axis.text  = element_text(size=14, face='bold'),
      axis.title = element_text(size=16, face='bold'),
    )
}


G = ggplot( data=df) +
    aes(x=cohort, y=value, color=var, label=vartext) +
    scale_x_continuous(breaks=seq(1935,1982,5) ) +
    scale_y_continuous(limits=c(0,55),
                       breaks=seq(0,50,10)) +
    geom_line(size=2) +
    geom_text( nudge_x=1, size=5, hjust=0 ) +
    guides(color=FALSE) +
    labs(x='Woman\'s Year of Birth',
         y='Percent of Women',
         title='Children Ever Born by Mother\'s Birth Year',
         subtitle='Netherlands',
         caption='Source: http://tinyurl.com/ned-cfr') +
    theme_carl() +
    transition_reveal(cohort)

animate(
  G,
  fps      = 10,
  duration = 15,
  width    = 500,
  height   = 400,
  renderer = gifski_renderer('ned-cfr.gif')
)
