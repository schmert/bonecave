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
 
ex_data = bind_rows(female_life_table,
                    male_life_table) %>% 
          filter(Year == max(Year)) %>% 
          select(Year, Sex, Age, ex)

# merge in population data for the
# latest available mortality year

latest_year = ex_data$Year[1]

data = pop %>% 
        filter(Year == latest_year) %>% 
        left_join(ex_data) %>% 
        ungroup()


#####################################
# the algorithm below smooths out the
# e(x) and N(x) functions before 
# binning population by integer
# values of life exp. 0,1,2,...
#####################################

target_ex = 0:99

pop_by_ex = tibble()

for (this_sex in c('Female','Male')) {
  
  tmp = filter(data,Sex==this_sex)
  
  N  = pull(tmp,N)
  ex = pull(tmp,ex)
  
  deltax = .01
  
  # very fine age grid
  xfine = seq(0,111,deltax) 

  # midpoints of the fine age intervals
  xmid = seq(deltax/2, 111-deltax/2, deltax)
  
  # population counts at the midpoints of the
  # fine age intervals
  
  Nfine = approx(x = 0:111,
                 y= cumsum( c(0,N)),
                 xout = xfine)$y %>% diff()
  
  # life expectancies at the midpoints of the
  # fine age intervals
  
  efine = approx(x=0:110,
                 y=ex,
                 xout=xmid,
                 yright=last(ex))$y
 
  max_ex = floor( max(efine) )
  
  ex_vals = seq(0,max_ex)
  
  # for each ex value, add up the population
  # in the fine grids corresponding to that value
  
  Nvals = sapply(ex_vals, function(age) {
            sum( Nfine[ (efine >= age) & (efine < age+1) ])
  })
  
  # append this sex's data
  
  pop_by_ex = pop_by_ex %>% 
              bind_rows(tibble(
                        sex=this_sex, 
                        ex = ex_vals, 
                        N  = Nvals))
  
}

#.................................................
# Make a "population pyramid" by remaining
# life expectancy
#.................................................
#
z = pop_by_ex %>% 
     mutate(N = ifelse(sex=='Male',-1,+1) * N)

text_df = tribble(
  ~sex, ~ex, ~N,
  'Female', 3, +1.7e6,
  'Male'  , 3, -1.9e6
)

hues = c('red', 'royalblue')

G = ggplot(data=z) +
  aes(x=ex, y=N, color=sex, fill=sex) +
  geom_bar(stat='identity',width=.40) +
  labs(x='Remaining Life Expectancy (yrs)',
       y='Population',
       title='US Population 2022',
       subtitle='by remaining period life expectancy',
       caption='Human Mortality Database\nhttps://mortality.org\n@CSchmert') +
  scale_x_continuous(
    breaks=seq(0,90,10),
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
       filename=here('US-mortality','US-population-by-ex.png'),
       width=6, height=8, units='in')
