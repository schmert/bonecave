library(tidyverse)
library(tidycensus)

rm(list = ls())

# selected_state = 'FL'
# pop_axis_breaks = seq(-500e3,+500e3, 250e3)
# pop_axis_labels = c('500K','250K','0','250K','500K')
# pop_axis_ref_value = 250e3

selected_state = 'CA'
pop_axis_breaks = seq(-1500e3,+1500e3, 500e3)
pop_axis_labels = c('1.5M','1M','0.5M','0', '0.5M', '1M', '1.5M')
pop_axis_ref_value = 1e6

## legislative info

legis = read_csv('legislators-current.csv') %>% 
     filter(type=='rep', state %in% state.abb) %>% 
     select(gender:district,party) %>% 
     rename(dist_num = district)

head(legis)

## census info by district
# get API key
my_api_key = scan('C:/Users/Carl/Dropbox/my-API-key.txt', what='character')

census_api_key(my_api_key)  # get your key at http://api.census.gov/data/key_signup.html

data(fips_codes)

fips = fips_codes %>% 
  group_by(state) %>% 
  slice(1) %>% 
  select(state, state_name, state_code) %>% 
  mutate(state_num = as.numeric(state_code))

# the small data frames below are crosswalks 
# that give details for ACS variable names

Table1_info = tribble(
  ~variable, ~label, ~L, ~H, ~sex, ~race,
'B01001_003',	'Under 5 years' , 0 , 4, 'male', 'all',
'B01001_004',	'5 to 9 years'  , 5,  9, 'male', 'all',
'B01001_005',	'10 to 14 years' ,10, 14, 'male', 'all',
'B01001_006',	'15 to 17 years' ,15,17,'male', 'all',
'B01001_007',	'18 and 19 years' ,18,19,'male', 'all',
'B01001_008',	'20 years' ,20,20,'male', 'all',
'B01001_009',	'21 years' ,21,21, 'male', 'all',
'B01001_010',	'22 to 24 years' ,22,24,'male', 'all',
'B01001_011',	'25 to 29 years' ,25,29,'male', 'all',
'B01001_012',	'30 to 34 years' ,30,34,'male', 'all',
'B01001_013',	'35 to 39 years' ,35,39,'male', 'all',
'B01001_014',	'40 to 44 years' ,40,44,'male', 'all',
'B01001_015',	'45 to 49 years' ,45,49,'male', 'all',
'B01001_016',	'50 to 54 years' ,50,54,'male', 'all',
'B01001_017',	'55 to 59 years' ,55,59,'male', 'all',
'B01001_018',	'60 and 61 years' ,60,61,'male', 'all',
'B01001_019',	'62 to 64 years' ,62,64,'male', 'all',
'B01001_020',	'65 and 66 years' ,65,66,'male', 'all',
'B01001_021',	'67 to 69 years' ,67,69,'male', 'all',
'B01001_022',	'70 to 74 years' ,70,74,'male', 'all',
'B01001_023',	'75 to 79 years' ,75,79,'male', 'all',
'B01001_024',	'80 to 84 years' ,80,84,'male', 'all',
'B01001_025',	'85 years and over',85,Inf,'male', 'all',

'B01001_027',	'Under 5 years' , 0 , 4, 'female', 'all',
'B01001_028',	'5 to 9 years'  , 5,  9, 'female', 'all',
'B01001_029',	'10 to 14 years' ,10, 14, 'female', 'all',
'B01001_030',	'15 to 17 years' ,15,17,'female', 'all',
'B01001_031',	'18 and 19 years' ,18,19,'female', 'all',
'B01001_032',	'20 years' ,20,20,'female', 'all',
'B01001_033',	'21 years' ,21,21, 'female', 'all',
'B01001_034',	'22 to 24 years' ,22,24,'female', 'all',
'B01001_035',	'25 to 29 years' ,25,29,'female', 'all',
'B01001_036',	'30 to 34 years' ,30,34,'female', 'all',
'B01001_037',	'35 to 39 years' ,35,39,'female', 'all',
'B01001_038',	'40 to 44 years' ,40,44,'female', 'all',
'B01001_039',	'45 to 49 years' ,45,49,'female', 'all',
'B01001_040',	'50 to 54 years' ,50,54,'female', 'all',
'B01001_041',	'55 to 59 years' ,55,59,'female', 'all',
'B01001_042',	'60 and 61 years' ,60,61,'female', 'all',
'B01001_043',	'62 to 64 years' ,62,64,'female', 'all',
'B01001_044',	'65 and 66 years' ,65,66,'female', 'all',
'B01001_045',	'67 to 69 years' ,67,69,'female', 'all',
'B01001_046',	'70 to 74 years' ,70,74,'female', 'all',
'B01001_047',	'75 to 79 years' ,75,79,'female', 'all',
'B01001_048',	'80 to 84 years' ,80,84,'female', 'all',
'B01001_049',	'85 years and over',85,Inf,'female', 'all'
)

TableH_info = tribble(
  ~variable, ~label, ~L, ~H, ~sex, ~race,
'B01001H_003', 'Under 5 years', 0, 4, 'male', 'WNH',
'B01001H_004', '5 to 9 years' , 5, 9, 'male', 'WNH',
'B01001H_005', '10 to 14 years' ,  10, 14, 'male', 'WNH',
'B01001H_006', '15 to 17 years' , 15, 17, 'male', 'WNH',
'B01001H_007', '18 and 19 years' , 18, 19, 'male', 'WNH',
'B01001H_008', '20 to 24 years' , 20, 24, 'male', 'WNH',
'B01001H_009', '25 to 29 years' , 25, 29, 'male', 'WNH',
'B01001H_010', '30 to 34 years' , 30, 34, 'male', 'WNH',
'B01001H_011', '35 to 44 years' , 35, 44, 'male', 'WNH',
'B01001H_012', '45 to 54 years' , 45, 54, 'male', 'WNH',
'B01001H_013', '55 to 64 years' , 55, 64, 'male', 'WNH',
'B01001H_014', '65 to 74 years' , 65, 74, 'male', 'WNH',
'B01001H_015', '75 to 84 years' , 75, 84, 'male', 'WNH',
'B01001H_016', '85 years and over',  85, Inf, 'male', 'WNH',

'B01001H_018', 'Under 5 years', 0, 4, 'female', 'WNH',
'B01001H_019', '5 to 9 years' , 5, 9, 'female', 'WNH',
'B01001H_020', '10 to 14 years' ,  10, 14, 'female', 'WNH',
'B01001H_021', '15 to 17 years' , 15, 17, 'female', 'WNH',
'B01001H_022', '18 and 19 years' , 18, 19, 'female', 'WNH',
'B01001H_023', '20 to 24 years' , 20, 24, 'female', 'WNH',
'B01001H_024', '25 to 29 years' , 25, 29, 'female', 'WNH',
'B01001H_025', '30 to 34 years' , 30, 34, 'female', 'WNH',
'B01001H_026', '35 to 44 years' , 35, 44, 'female', 'WNH',
'B01001H_027', '45 to 54 years' , 45, 54, 'female', 'WNH',
'B01001H_028', '55 to 64 years' , 55, 64, 'female', 'WNH',
'B01001H_029', '65 to 74 years' , 65, 74, 'female', 'WNH',
'B01001H_030', '75 to 84 years' , 75, 84, 'female', 'WNH',
'B01001H_031', '85 years and over',  85, Inf, 'female', 'WNH'
)


# data for all races (more age detail)
P1 = get_acs(geography = 'congressional district',
             state=selected_state,
            variables = Table1_info$variable,
            year=2019,
            output = 'tidy') %>% 
      mutate(state_num = as.numeric(substr(GEOID,1,2)),
             dist_num  = as.numeric(substr(GEOID,3,4)))   %>% 
      left_join(fips) %>% 
      left_join(Table1_info)


## merge political info and aggregate
## after this operation, rawpop is a 23x1 vector
## for groups starting at ages
##  0  5 10 15 18 20 21 22 25 30 35 40 45 50 55 60
##  62 65 67 70 75 80 85

D1 = left_join(legis, P1, by=c('state','dist_num')) %>% 
      select(-NAME,-moe,-state_num, -state_code) %>% 
      group_by(party,L,H,sex,race) %>% 
      summarize(pop=sum(estimate)) %>%
      ungroup() %>% 
      select(-L,-H) %>% 
      nest(rawpop = pop) 


# data for white non-Hispanics (less age detail)
PH = get_acs(geography = 'congressional district',
             state=selected_state,
             variables = TableH_info$variable,
             year=2019,
             output = 'tidy') %>% 
  mutate(state_num = as.numeric(substr(GEOID,1,2)),
         dist_num  = as.numeric(substr(GEOID,3,4)))   %>% 
  left_join(fips) %>% 
  left_join(TableH_info)


## merge political info and aggregate
## after this operation, rawpop is a 14x1 vector
## for groups starting at ages
##   0  5 10 15 18 20 25 30 35 45 55 65 75 85

DH = left_join(legis, PH, by=c('state','dist_num')) %>% 
  select(-NAME,-moe,-state_num, -state_code) %>% 
  group_by(party,L,H,sex,race) %>% 
  summarize(pop=sum(estimate)) %>%
  ungroup() %>% 
  select(-L,-H) %>% 
  nest(rawpop = pop) 

######### convert both data sets to common 5-yr age groups
#  18 groups: 0-4, 5-9, 10-14, ... , 80-84, 85+

# table 1 had 23 age groups. Aggregate to 18

### matrix W1 such that 
###   W1 %*% [vector of 23 pops from Table 1] = [vector of 18 5-yr pop groups]

Lstar = seq(0,85,5)
L     = sort( unique(Table1_info$L))

W1 = outer(Lstar, L, function(x,y) 1*(x<=y)*(y < x+5))
dimnames(W1) = list(Lstar,L)

sm5 = function(x) { as.vector(W1 %*% unlist(x))}

D1 = D1 %>% 
      filter(!is.na(sex), !is.na(race)) %>% 
      mutate(smoothpop = lapply(rawpop, sm5))


## function to find smoothest set of 
## 18 5-yr pop totals that exactly match the 14
## totals in Table H

# N must be a 14-vector for groups with L as
# in Table H: 0 5 10 15 18 20 25 30 35 45 55 65 75 85

# first have to define a 13 x 14 matrix to aggregate the
# 15-17 and 18-19 groups in the race-specific data
# CURSE YOU, CENSUS BUREAU!

L1 = c(seq(0,30,5), seq(35,85,10))
H1 = c( tail(L1,-1), Inf)
L2 = c(0, 5, 10, 15, 18, 20, 25, 30, 35, 45, 55, 65, 75, 85)
H2 = c( tail(L2,-1), Inf)

A =  outer(seq(L1),seq(L2), 
    function(i,j) 1*(L1[i]<=L2[j])*(H1[i] >= H2[j]))
dimnames(A) = list(L1,L2)

# W is an 13 x 18 matrix to collapse the a hypothetical
# pop disaggregated into 18 5-year groups into the
# data 13 groups observed in the race-specific table
# CURSE YOU, CENSUS BUREAU!

Lstar = seq(0,85,5)
Hstar = c( tail(Lstar,-1), Inf)

W =  outer(seq(L1),seq(Lstar), 
           function(i,j) 1*(L1[i]<=Lstar[j])*(H1[i] >= Hstar[j]))
dimnames(W) = list(L1,Lstar)

# 16 x 18 2nd-differencing matrix for 18 groups
D2 = diff(diag(length(Lstar)), diff=2)

# penalty for non-linearity in 5-year population totals
P = crossprod(D2)

# solve for smoothest set of 18 5-yr pops that
# matches the Table H raw totals

Z = rbind( cbind(P, -t(W)),
           cbind(W, diag(0,13)))

sm5H = function(rawpopH) {
    y = A %*% unlist(rawpopH)  # now 13 groups
    v = c(rep(0,18), y)
    soln = solve( Z, v)
    return( soln[1:18])
}


DH = DH %>% 
  filter(!is.na(sex), !is.na(race)) %>% 
  mutate(smoothpop = lapply(rawpop, sm5H))


## FINALLY. (CURSE YOU, CENSUS BUREAU!)

POP = rbind(D1, DH)


###########################################
# NOW PLOT PYRAMIDS
###########################################

# unnest and widen, to make plotting more convenient


plot_data = POP %>% 
              select(-rawpop) %>% 
              unnest(smoothpop) %>% 
              transform(L=Lstar) %>% 
              mutate(smoothpop = ifelse(sex=='male',-1,+1)*smoothpop ) %>% 
              pivot_wider(names_from=race,values_from =smoothpop ) 
  
# add a factor for age group
age = factor(Lstar, 
             levels=Lstar, 
             labels= c(paste(head(Lstar,-1),head(Hstar,-1),sep='-'),
                         '85+'))

plot_data = plot_data %>% 
             transform(age=age) %>%
             mutate( rep=factor(party,
                                levels=c('Democrat','Republican'),
                                labels=paste('Represented by a',c('Democrat','Republican'))) )



## plot all

G = ggplot(data=plot_data) +
  aes(x = age, y=all) +
  geom_bar(data=filter(plot_data,sex=='male'),
           stat='identity', fill='cornflowerblue',alpha=.70, width=.90,
           color='black',lwd=0.4) +
  geom_bar(data=filter(plot_data,sex=='female'),
           stat='identity', fill='lightcoral', alpha=.70, width=.90,
           color='black', lwd=0.4) +
  scale_y_continuous(breaks=pop_axis_breaks,
                     labels=pop_axis_labels,
                     minor_breaks = NULL) +
  coord_flip() +
  facet_grid(~rep) +
  theme_bw() 

G = G + 
  geom_bar(data=filter(plot_data,sex=='male'),aes(x=age,y=WNH,fill=sex),
           stat='identity', alpha=1, fill='blue', width=.90) +
  geom_bar(data=filter(plot_data,sex=='female'),aes(x=age,y=WNH,fill=sex),
           stat='identity', alpha=1, fill='red', width=.90) +
  geom_hline(yintercept = c(-1,1)*pop_axis_ref_value, 
             lty='dotted', color='black',alpha=.80) +
  labs(title=paste(selected_state,'residents: US House Representation by Age, Sex, and Race/Ethnicity'),
       subtitle='117th Congress',
       y='Population', x='Age Group',
       caption='Source: US Census Bureau ACS 2015-2019') +
  geom_text(x=18, y=-6e6, label='Male', color='blue') +
  geom_text(x=18, y=+6e6, label='Female', color='red') +
  geom_text(x=1, y=0,label='White Non-Hispanics', color='white') +
  geom_text(x=1, y=5.6e6,label='Total', color='black') 

  
  
ggsave(plot=G, filename=paste0('population-pyramids-by-party-',selected_state,'.png'),
       height=6, width=12, dpi=300)

####################



