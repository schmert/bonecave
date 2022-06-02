library(wpp2019)
library(tidyverse)
library(showtext)

font_add_google("Roboto", "Roboto")
showtext_auto() 


theme_carl <- function () { 
  theme_bw(base_size=44, base_family="Roboto") %+replace% 
    theme( plot.caption= element_text(size=30, hjust=1, color='red'),
           panel.grid  = element_line(color='lightgrey', size=0.2),
           axis.text   = element_text(size=40, face='bold'),
           axis.ticks  = element_line(color='grey', size=1)  
         )
}

# read WPP data
# 
data(popF, popM, popMprojMed, popFprojMed)

L          = seq(0,100,5)
agecat     = paste(L,L+4,sep='-')
agecat[21] = '100+'

worldpop = rbind(popF , popM)  %>% 
              filter(name == 'World') %>%
              mutate(age=factor(age,levels=agecat)) %>% 
              group_by(age) %>% 
              summarize_at(-(1:2),sum) %>% 
              ungroup()

names(worldpop) = c('age', paste0('n',seq(1950,2020,5)) )


proj_to_2025 = rbind(popMprojMed, popFprojMed)   %>% 
                  filter(name == 'World') %>% 
                  mutate(age=factor(age,levels=agecat)) %>%
                  group_by(age) %>% 
                  summarize_at(-(1:2),sum) %>% 
                  ungroup() %>% 
                  select(age, n2025=`2025`)

frac_below = function(x) {cumsum(x)/sum(x)}

data = full_join(worldpop, proj_to_2025) %>% 
         add_column(L=L,H=L+5, .after='age') %>% 
         mutate(across(starts_with('n'), frac_below))


# for a year that's an integer multiple of 5, find the 
# fraction born before 6 Feb 1952

QE_frac = function(year) {
  
  Q = 1952 + 37/366
  Y = year + .50 
  
  astar = Y - Q
  
  P = data %>% 
       pull( paste0('n', year))
  
  result = approx(x=c(L,105), y=c(0,P), xout=astar)$y
  return(result)
}

yy = seq(1955,2025, 5)
ff = sapply(yy , QE_frac)

tmp = tibble( year = c(1952,yy),
              pct = 100*c(0,ff))

ggplot(data=tmp) +
  aes(x=year, y=pct) +
  geom_ribbon(aes(x=year,ymin=0,ymax=pct), fill='navy',alpha=.90) +
  geom_line(color='red', size=2.5) +
  geom_text(x=1995, y=28,label='Born After', 
            size=24, color='white') +
  geom_text(x=1995, y=22,label='Elizabeth II\'s Coronation', 
            size=24, color='white') +
  theme_carl() +
  labs(x='Year', y='Percent of World Population',
       caption='Source: UN World Population Prospects + @cschmert',
       title='70th JUBILEE') +
  geom_text(x=1953,y=93, label='% of World\'s Living Population',hjust=0,size=24) +
  geom_text(x=1953,y=87, label='who have only known',hjust=0,size=24) +
  geom_text(x=1953,y=81, label='1 English monarch',hjust=0,size=24) +
  scale_y_continuous(limits=c(0,100), breaks=0:4*25,minor_breaks = NULL)

ggsave(filename='qe2-jubilee.png', height=8, width=8, dpi=300)
