rm( list=ls() )
graphics.off()
windows(record=TRUE)

library(read.dbc)
library(tidyverse)

#---------------------------------------
# data processed state by state
#---------------------------------------

state = c("AC", "AL", "AM", "AP", "BA", 
          "CE", "DF", "ES", "GO", "MA", 
          "MG", "MS", "MT", "PA", "PB", 
          "PE", "PI", "PR", "RJ", "RN", "RO", 
          "RR", "RS", "SC", "SE", "SP", "TO")

region = c('N','NE','N','N','NE',
           'NE','CW','SE','CW','NE',
           'SE','CW','CW','N','NE',
           'NE','NE','S','SE','NE','N',
           'N','S','S','NE','SE','CW'
          )

geo = data.frame( i=seq(state), state, region)

#---------------------------------------

# output_code is 
#   %j for day of year (1-366),
#   %w for day of week (0-6), 
#   %m for month number (1-12),
# etc 

process_date = function(x, output_code='%j') {
  tmp = as.Date(x, '%d%m%Y') %>%
    strftime(format= output_code) %>%
    as.numeric()
  return(tmp)
}


#--------------------------------------
# assemble death summaries, looping
# over years and states
#--------------------------------------

B = data.frame()  # summary

years = 2012:2017
nyrs  = length(years)

for (this_year in years) {
  for (i in seq(state)) {

    this_state  = state[i]
    this_region = region[i]
    print(paste(this_year,this_state))
    
    fname = paste0('./SINASC-raw-data/',
                   ifelse(this_year==2017,'DNP','DN'),
                   this_state,this_year,'.dbc')
    
    tmp   = read.dbc(fname) %>%
               mutate(state  = this_state,
                      region = this_region,
                      year   = this_year,
                      delivery = factor(PARTO,
                                        levels=c(1,2,9),
                                        labels=c('Vaginal','Cesarean','Unknown')),
                      month  = process_date(DTNASC,'%m'),
                      dom    = process_date(DTNASC,'%d'),
                      doy    = process_date(DTNASC,'%j'),
                      dow    = process_date(DTNASC,'%w')
                      ) %>%
               select(state:dow)


    keep = tmp %>%
              group_by(year,month, dom, doy, dow) %>%
              summarize(births=n()) %>%
              ungroup()
    
     B = rbind(B, keep)
  
  } # this_state
} # this_year

# number of days in each of the 60 months Jan 2012-Dec 2017
ndays = rep( c(31,28,31,30,31,30,31,31,30,31,30,31), 6)
ndays[c(2,50)] = 29

write.csv(B, file='births-2012-2017-summary.csv')

# sum by year and month
df = B %>%
       mutate(M = 12*(year-2012) + month) %>%
       group_by(M) %>%
       summarize(births=sum(births))

df$daily_births = df$births / ndays

mletter = c('J','F','M','A','M','J','J','A','S','O','N',"D")

ggplot( data=df, aes(x=M+.5, y=daily_births)) +
    geom_point() +
    geom_line() +
    scale_x_continuous(
      breaks=seq(1,73,12),
      minor_breaks = NULL,
      labels=2012:2018) +
    geom_text(aes(x= M+.5, y=6700, 
              label=rep(mletter,6)),
              size=3,inherit.aes = FALSE) +
    geom_vline(xintercept=seq(1,75,3), lty=2, color='grey') +
    geom_vline(xintercept=seq(1,73,12), lty=1, color='orangered') +
    labs(title='Average Daily Births by Month and Year, Brazil 2012-2017',
         caption = 'Source: SINASC/Datasus http://www.datasus.gov.br',
         x='Month',
         y='Births per Day') +
    geom_text(aes(x=59, y=7200, label='Zika'), color='red', size=7) +
    theme_bw()

ggsave(file='average-daily-births-by-month-Brazil-2012-2017.png',
       width=11, height=8.5)

