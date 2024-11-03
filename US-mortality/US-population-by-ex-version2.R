#.....................................................
# Carl Schmertmann
# 18 Sep 2024
# 
# US population by remaining sex and remaining
# (period) life expectancy
# 
# data from Human Mortality Database
# https://mortality.org
#.....................................................

library(tidyverse)
library(HMDHFDplus)
library(here)       # points to parent directory, bonecave/

# YOUR HMD credentials must already be in objects 
# called un and pw (username and password, resp.)

# read US population by age and sex from HMD
# Calculate and keep mid-year population estimates, which 
# are the average of 1 Jan (...1) and 31 Dec (...2) columns
# for each sex

pop = readHMDweb(CNTRY='USA', item='Population', 
                 username=un, password=pw) %>% 
      group_by(Year, Age) %>% 
      summarize(Male = (Male1 + Male2)/2,
                Female = (Female1 + Female2)/2) %>% 
      ungroup() %>% 
      pivot_longer(cols=c(Male,Female),
                   names_to = 'Sex',
                   values_to = 'N')


# read US period life tables from HMD

female_life_table = readHMDweb(CNTRY='USA', 
                        item='fltper_1x1', 
                        username=un, password=pw) %>% 
                     add_column(Sex='Female')

male_life_table = readHMDweb(CNTRY='USA', 
                               item='mltper_1x1', 
                               username=un, password=pw)  %>% 
                     add_column(Sex='Male')

# keep only the age-specific ex values (remaining
# life expectancies) for the latest available
# year
 
mort_data = bind_rows(female_life_table,
                    male_life_table) %>% 
          filter(Year == max(Year)) %>% 
          select(Year, Sex, Age, mx) %>% 
          group_by(Year,Sex) %>% 
          arrange(Age) %>% 
          mutate(Hx = head( cumsum(c(0,mx)),-1)) 

#.. function to calculate the (period) survival
# prob. of living y = [0,1), [1,2), ... [110,111) more
# years, for a current x+0.5 year old, at current
# ASMRS

surv = function(x,s='Male') {
  # mort rates over age intervals [x,x+1), [x+1,x+2), ..., [110,Inf)
  mx = filter(mort_data, Sex==s, Age >= x) %>% pull(mx)
  
  # cumul mort risk from exact age x to exact age (x, x+1, x+2, ..., 111)
  Hx = cumsum(c(0,mx))  
  
  # cumul mort risk from exact age x+1/2 to exact ages (x+1/2, x+3/2, ..., 110.5)
  Hx_offset = (head(Hx,-1) + tail(Hx,-1)) / 2 
  
  # probs of surviving [0,1), [1,2), ...,[109-x) more years at period rates
  p = -diff( exp(-Hx_offset))
  
  # (very) slight cleanup for sums that are .9998, etc
  p = prop.table(p)
  
  full_pvec = rep(0, 110)
  names(full_pvec) = 0:109
  full_pvec[paste(0:(109-x))] = p
  
  return(full_pvec)
}

latest_year = mort_data$Year[1]

population = function(s) {
  pop %>% 
    filter(Year == latest_year, Sex==s) %>% 
    pull(N) %>% 
    head(110)
}

N     = population('Male')
Pmat  = sapply(0:109, surv, s='Male')
Nmale = Pmat %*% N   # male pop by expected years surviving

N       = population('Female')
Pmat    = sapply(0:109, surv, s='Female')
Nfemale = Pmat %*% N   # female pop by expected years surviving

plot(0:109, Nmale, type='l', ylim=c(0,2.4e6),lwd=2,
     xlab='Expected Years of Life Remaining',
     ylab='Number of US residents')
lines(0:109, Nfemale, col='red',lwd=2)

# now convert to a pyramid...


#.................................................
# Make a "population pyramid" by remaining
# life expectancy
#.................................................

df =   tibble(sex='Male', ex=0:109, N=-Nmale) %>% 
    bind_rows(
      tibble(sex='Female', ex=0:109, N=Nfemale)
      )  

text_df = tribble(
  ~sex, ~ex, ~N,
  'Female', 98, +1.7e6,
  'Male'  , 98, -1.9e6
)

hues = c('red', 'royalblue')

G = ggplot(data=df) +
  aes(x=ex, y=N, color=sex, fill=sex) +
  geom_bar(stat='identity',width=.40) +
  labs(x='Years of Life Remaining',
       y='Population',
       title='US Population 2022',
       subtitle='by expected years of remaining life at 2022 mortality rates',
       caption='Human Mortality Database\nhttps://mortality.org\n@CSchmert') +
  scale_x_continuous(
    breaks=seq(0,110,10),
    expand = c(0.01,0),
    sec.axis=dup_axis(name='')) +
  scale_y_continuous(breaks=seq(-3e6,3e6,1e6),
                     labels=c(paste0(3:1,'M'),'0',paste0(1:3,'M'))) +
  scale_color_manual(values=hues) +
  scale_fill_manual(values=hues) +
  theme_bw() +
  coord_flip() +
  guides(color='none',fill='none') +
  geom_text(data=text_df, aes(label=sex), fontface='bold',size=5) +
  theme(axis.title = element_text(face='bold', size=12),
        axis.text  = element_text(face='bold', size=10),
        plot.title = element_text(face='bold'))

ggsave(plot=G,
       filename=here('US-mortality','US-population-by-ex-version-2.png'),
       width=6, height=8, units='in')
