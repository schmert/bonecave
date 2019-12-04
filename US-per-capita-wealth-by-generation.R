##########################################################
# Real US per capita assets and liabilities, by generation
#
# population data from HMD (https://www.mortality.org/hmd/USA/STATS/Population.txt)
# nominal wealth data from Federal Reserve (eg https://www.federalreserve.gov/releases/z1/dataviz/dfa/distribute/table/#quarter:119;series:Net%20worth;demographic:generation;population:all;units:shares)
# price index from https://fred.stlouisfed.org/series/CPALTT01USQ661S
##########################################################

library(tidyverse)
library(lubridate)

graphics.off()

options(scipen=999)

make_year = function(qtr) {
  tmp = strsplit(qtr,':') %>% 
          unlist() %>%
          matrix(nrow=2)
  yy = as.numeric(tmp[1,])
  rr = case_when(
        tmp[2,] == 'Q1' ~ .125,
        tmp[2,] == 'Q2' ~ .375,
        tmp[2,] == 'Q3' ~ .625,
        tmp[2,] == 'Q4' ~ .875
  )
  return(yy + rr)
}

# dollar values in millions (/ 1e6) 
W = read_csv('US-wealth-levels-by-generation.csv') %>%
      mutate(date = make_year(Date))

names(W) = str_replace_all(names(W),' ','_')

P = read_table('US-population.txt',skip=2) %>%
      transform(Age = 0:110) %>%
      mutate(yob = Year-Age,
             gen = case_when(
                       yob %in% 1946:1964 ~ 'BabyBoom',
                       yob %in% 1965:1980 ~ 'GenX',
                       yob %in% 1981:1996 ~ 'Millennial',
                       TRUE ~ 'Other'
             )) %>% 
      select(Year,yob,gen, pop=Total) %>%
      group_by(Year,gen) %>%
      summarize( genpop = sum(pop)) %>%
      mutate(med_gen_age = case_when(
        gen == 'BabyBoom'   ~ Year-1955,
        gen == 'GenX'       ~ Year-1972.5,
        gen == 'Millennial' ~ Year-1988.5,
        TRUE ~ NA_real_
      )) %>% 
      filter(gen %in% c('BabyBoom','GenX','Millennial'))

## price index data

CPI = read_csv('CPI-from-StLouis-Fed.csv') 
names(CPI) = c('DDMMYY','index') 

CPI = CPI %>%
       mutate(mult = tail(index,1)/index,
              date = decimal_date(parse_date_time(DDMMYY,
                                                  orders='dmy')))

CPI$date[CPI$date > 2020] = CPI$date[CPI$date > 2020]-100

# interpolated inflation adjustment multiplier function
adjust = approxfun(x=CPI$date, y=CPI$mult)

# convert nominal amounts in W into mid-2019 dollars
# clumsy but effective
vname = names(W)[3:14]
for (v in vname) {
  W[[v]] = W[[v]] * adjust(W$date)
} 



# add the generational populations to the wealth data
# and calculate per capita figures

big = W %>%
        mutate(Year = floor(date)) %>%
        left_join(P, by=c('Year','Category'='gen')) %>%
        mutate(Assets_per_capita      = 1e6 * Assets/genpop,
               Liabilities_per_capita = 1e6* Liabilities/genpop,
               Net_worth_per_capita   = Assets_per_capita-Liabilities_per_capita) %>% 
        select(date,Category, contains('_per_capita'), genpop, med_gen_age)

pdf(file='US-wealth-by-generation.pdf',
    width=11, height=8.5)

  G = ggplot(data=big) +
     aes(x=date, y=Assets_per_capita, color=Category, group=Category) +
     geom_point(lwd=1.5) +
     theme_bw() 
  
  print(G)
  
  G = ggplot(data=big) +
    aes(x=date, y=Liabilities_per_capita, group=Category,color=Category) +
    geom_line(lwd=1.5) +
    theme_bw()
  
  print(G)
  
  G = ggplot(data=big) +
    aes(x=date, y=Net_worth_per_capita, group=Category,color=Category) +
    geom_line(lwd=1.5) +
    theme_bw()
  
  print(G)
  
  G = ggplot(data=big) +
    aes(x=med_gen_age, y=Assets_per_capita, group=Category,color=Category) +
    scale_x_continuous(limits=c(20,70)) +
    geom_smooth(lwd=1.5,se=FALSE) +
    labs(x='Median Age of Cohort') +
    theme_bw()
  
  print(G)
  
  G = ggplot(data=big) +
    aes(x=med_gen_age, y=Liabilities_per_capita, group=Category,color=Category) +
    scale_x_continuous(limits=c(20,70)) +
    geom_smooth(lwd=1.5,se=FALSE) +
    labs(x='Median Age') +
    theme_bw()
  
  print(G)
  
  G = ggplot(data=big) +
    aes(x=med_gen_age, y=Net_worth_per_capita, group=Category,color=Category) +
    scale_x_continuous(limits=c(20,70)) +
    geom_smooth(lwd=1.5, se=FALSE) +
    labs(x='Median Age') +
    theme_bw()
  
  print(G)
  
  # in panels by variable
  
  long = big %>% 
          gather(key='measure', value='amt', -date, -Category, -genpop, -med_gen_age)
  
  G = ggplot(data=long) +
    aes(x=med_gen_age, y=amt, group=Category,color=Category) +
    scale_x_continuous(limits=c(20,70)) +
    geom_smooth(lwd=1.5, se=FALSE) +
    theme_bw() +
    labs(x='Median Age') +
    facet_grid(. ~ measure)
  
  print(G)

dev.off()


# main plot

theme_carl <- function () { 
  theme_bw(base_size=13) %+replace% 
    theme(
      title      = element_text(size=20, face='bold'),
      plot.caption  = element_text(size=10, face='italic',hjust=0),
      axis.text  = element_text(size=15, face='bold'),
      axis.title = element_text(size=15, face='bold')
    )
}

G = ggplot(data=big) +
  aes(x=med_gen_age, y=Net_worth_per_capita, group=Category,color=Category) +
  scale_x_continuous(limits=c(20,70)) +
  scale_y_continuous(breaks=seq(0,800000,200000), minor_breaks = NULL) +
  geom_smooth(lwd=3, se=FALSE, alpha=.90) +
  guides(color=FALSE) +
  labs(x='Median Age') +
  theme_carl() +
  labs(title='Per Capita Net Worth in 2019 dollars',
       caption='Sources:\nFederal Reserve Board of Governors (wealth distribution)\nSt. Louis Federal Reserve (price index)\nHuman Mortality Database (annual populations by age and generation)')

print(G)

