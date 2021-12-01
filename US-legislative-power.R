#..............................................................
# Legislative Power of US Citizens, by state
# Data from Michael Fay, letter to Amstat News, Nov 2021
# 
# Carl Schmertmann
# 01 Dec 2021
# 
# assumes 
#   1/2 of "legislative power" is in US Senate
#   1/2 in House of Reps
#   
# Legislative Power of state S =
#    (1/2) * (2 Sen)/100   + (1/2) * (#reps from state S)/435
#..............................................................
library(tidyverse)

# data from Fay's Table 1: state, 2020 population in millions,
# number of Reps after 2020 reapportionment, state's share of
# total legislative power, state's share of total population,
# ratio of power/pop

Fay = tribble(
  ~State,~Pop,~Reps,~PowerShare,~PopShare,~ratio,
  'District of Columbia',0.69,0,0,0.21,0,
  'California',39.58,52,6.98,11.93,0.58,
  'Texas',29.18,38,5.37,8.8,0.61,
  'Florida',21.57,28,4.22,6.5,0.65,
  'New York',20.22,26,3.99,6.09,0.65,
  'Pennsylvania',13.01,17,2.95,3.92,0.75,
  'Illinois',12.82,17,2.95,3.86,0.76,
  'Ohio',11.81,15,2.72,3.56,0.77,
  'Georgia',10.73,14,2.61,3.23,0.81,
  'Michigan',10.08,13,2.49,3.04,0.82,
  'North Carolina',10.45,14,2.61,3.15,0.83,
  'New Jersey',9.29,12,2.38,2.8,0.85,
  'Virginia',8.65,11,2.26,2.61,0.87,
  'Washington',7.72,10,2.15,2.33,0.92,
  'Arizona',7.16,9,2.03,2.16,0.94,
  'Massachusetts',7.03,9,2.03,2.12,0.96,
  'Tennessee',6.92,9,2.03,2.08,0.98,
  'Indiana',6.79,9,2.03,2.05,0.99,
  'Maryland',6.19,8,1.92,1.86,1.03,
  'Missouri',6.16,8,1.92,1.86,1.03,
  'Wisconsin',5.9,8,1.92,1.78,1.08,
  'Colorado',5.78,8,1.92,1.74,1.1,
  'Minnesota',5.71,8,1.92,1.72,1.12,
  'South Carolina',5.12,7,1.8,1.54,1.17,
  'Alabama',5.03,7,1.8,1.52,1.19,
  'Louisiana',4.66,6,1.69,1.4,1.2,
  'Kentucky',4.51,6,1.69,1.36,1.24,
  'Oklahoma',3.96,5,1.57,1.19,1.32,
  'Oregon',4.24,6,1.69,1.28,1.32,
  'Connecticut',3.61,5,1.57,1.09,1.45,
  'Utah',3.28,4,1.4,0.99,1.48,
  'Iowa',3.19,4,1.46,0.96,1.52,
  'Nevada',3.11,4,1.46,0.94,1.56,
  'Arkansas',3.01,4,1.46,0.91,1.61,
  'Mississippi',2.96,4,1.46,0.89,1.63,
  'Kansas',2.94,4,1.46,0.89,1.65,
  'New Mexico',2.12,3,1.34,0.64,2.1,
  'Idaho',1.84,2,1.23,0.55,2.22,
  'Nebraska',1.96,3,1.34,0.59,2.27,
  'West Virginia',1.8,2,1.23,0.54,2.27,
  'Hawaii',1.46,2,1.23,0.44,2.79,
  'New Hampshire',1.38,2,1.23,0.42,2.96,
  'Maine',1.36,2,1.23,0.41,2.99,
  'Rhode Island',1.1,2,1.23,0.33,3.72,
  'Delaware',0.99,1,1.11,0.3,3.73,
  'Montana',1.09,2,1.23,0.33,3.76,
  'South Dakota',0.89,1,1.11,0.27,4.17,
  'North Dakota',0.78,1,1.11,0.23,4.74,
  'Alaska',0.74,1,1.11,0.22,5.03,
  'Vermont',0.64,0,1.11,0.19,5.75,
  'Wyoming',0.58,1,1.11,0.17,6.4
) %>% 
  left_join(tibble(State=c(state.name,'District of Columbia'),
                   abb = c(state.abb,'DC')))   # join 2-letter abbreviations


ggplot(data=filter(Fay,abb != 'DC')) +
  aes(x = reorder(abb,ratio), y=ratio, label=abb) +
  geom_bar(stat='identity', width=.60, fill='orangered', alpha=.70) +
  scale_y_log10(breaks=c(0.50, 1, 2, 3, 4, 5,6), minor_breaks=NULL, limits=c(0.45,6.8)) +
  scale_x_discrete(breaks=NULL) +
  labs(x='', y='(Actual Power) / (Power with Equal Representation)\nIdeally all States=1',
       title='Deviations of Legislative Power from Equal Representation',
       subtitle = 'Under 2020 Census Apportionment for House of Representatives',
       caption='Source: Michael Fay, letter to AmStat News, Nov 2021') +
  theme_bw() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_text(size=12, face='bold'),
        ) +
  geom_text(size=2, nudge_y = .015 * ifelse(Fay[-1,]$ratio<1,-1,+1), fontface='bold') +
  coord_flip()

ggsave(filename='US-legislative-power.png')