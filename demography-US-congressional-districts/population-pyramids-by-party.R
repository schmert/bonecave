library(tidyverse)
library(tidycensus)

## legislative info

D = read_csv('legislators-current.csv') %>% 
     filter(type=='rep', state %in% state.abb) %>% 
     select(gender:district,party) %>% 
     rename(dist_num = district)

head(D)

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

# the small data frame below is a crosswalk to
# identify a 5-year age group for any of the 
# (sometimes more disaggregated) groups that 
# appear in the table for total populations
age_group_info = tribble(
  ~age_label, ~group,
  'Under 5 years',     '0-4',
  '5 to 9 years' ,     '5-9',
  '10 to 14 years',  '10-14',
  '15 to 17 years',  '15-19',
  '18 and 19 years', '15-19',
  '20 years',        '20-24',
  '21 years',        '20-24',
  '22 to 24 years',  '20-24',
  '25 to 29 years',  '25-29',
  '30 to 34 years',  '30-34',
  '35 to 39 years',  '35-39',
  '40 to 44 years',  '40-44',
  '45 to 49 years',  '45-49',
  '50 to 54 years',  '50-54',
  '55 to 59 years',  '55-59',
  '60 and 61 years', '60-64',
  '62 to 64 years',  '60-64',
  '65 and 66 years', '65-69',
  '67 to 69 years',  '65-69',
  '70 to 74 years',  '70-74',
  '75 to 79 years',  '75-79',
  '80 to 84 years',  '80-84',
  '85 years and over', '85+'
)

# convert group to a properly-ordered factor
L = seq(0,80,5)

age_group_info = age_group_info %>% 
                   mutate(group = factor(group, 
                          levels=c(paste0(L,'-',L+4),'85+')))

$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  
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

'B01001H_01', 'Under 5 years' , 
'B01001H_01', '5 to 9 years' , 
'B01001H_02', '10 to 14 years' , 
'B01001H_02', '15 to 17 years' , 
'B01001H_02', '18 and 19 years' , 
'B01001H_02', '20 to 24 years' , 
'B01001H_02', '25 to 29 years' , 
'B01001H_02', '30 to 34 years' , 
'B01001H_02', '35 to 44 years' , 
'B01001H_02', '45 to 54 years' , 
'B01001H_02', '55 to 64 years' , 
'B01001H_02', '65 to 74 years' , 
'B01001H_03', '75 to 84 years' , 
'B01001H_03', '85 years and over'
)

'$$$$$$$'$$'$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
'MALE -all
'01001_003'Under 5 years
'B01001_004'	5 to 9 years
'B01001_005'10 to 14 years
'B01001_006'	15 to 17 years
'B01001_007'18 and 19 years
'B01001_008'	20 years
'B01001_009'21 years
'B01001_010'	22 to 24 years
'B01001_011'25 to 29 years
'B01001_012'	30 to 34 years
'B01001_013'35 to 39 years
'B01001_014'	40 to 44 years
'B01001_015'45 to 49 years
'B01001_016'	50 to 54 years
'B01001_017'55 to 59 years
'B01001_018'	60 and 61 years
'B01001_019'62 to 64 years
'B01001_020'	65 and 66 years
'B01001_021'67 to 69 years
'B01001_022'	70 to 74 years
'B01001_023'75 to 79 years
'B01001_024'	80 to 84 years
'B01001_025'85 years and over
''
'FEMALE 'al'
'B01001_027'Under 5 years
'B01001_028'	5 to 9 years
'B01001_029'10 to 14 years
'B01001_030'	15 to 17 years
'B01001_031'18 and 19 years
'B01001_032'	20 years
'B01001_033'21 years
'B01001_034'	22 to 24 years
'B01001_035'25 to 29 years
'B01001_036'	30 to 34 years
'B01001_037'35 to 39 years
'B01001_038'	40 to 44 years
'B01001_039'45 to 49 years
'B01001_040'	50 to 54 years
'B01001_041'55 to 59 years
'B01001_042'	60 and 61 years
'B01001_043'62 to 64 years
'B01001_044'	65 and 66 years
'B01001_045'67 to 69 years
'B01001_046'	70 to 74 years
'B01001_047'75 to 79 years
'B01001_048'	80 to 84 years
'B01001_049'85 years and over
# 
# white-alone NON-HIST versions of these are in table B01001H_... NOT QUITE, B
#  BECAUSE THE $%$%$ AGE GROUPS ARE DIFFT IN THE RACE-SPECIFIC TABLES

# MALE
# B01001H_003	Under 5 years 
# B01001H_004	5 to 9 years 
# B01001H_005	10 to 14 years 
# B01001H_006	15 to 17 years 
# B01001H_007	18 and 19 years 
# B01001H_008	20 to 24 years 
# B01001H_009	25 to 29 years 
# B01001H_010	30 to 34 years 
# B01001H_011	35 to 44 years 
# B01001H_012	45 to 54 years 
# B01001H_013	55 to 64 years 
# B01001H_014	65 to 74 years 
# B01001H_015	75 to 84 years 
# B01001H_016	85 years and over 
# 
# FEMALE
# B01001H_018	Under 5 years 
# B01001H_019	5 to 9 years 
# B01001H_020	10 to 14 years 
# B01001H_021	15 to 17 years 
# B01001H_022	18 and 19 years 
# B01001H_023	20 to 24 years 
# B01001H_024	25 to 29 years 
# B01001H_025	30 to 34 years 
# B01001H_026	35 to 44 years 
# B01001H_027	45 to 54 years 
# B01001H_028	55 to 64 years 
# B01001H_029	65 to 74 years 
# B01001H_030	75 to 84 years 
# B01001H_031	85 years and over 




Table1_age_labels = c('Under 5 years', '5 to 9 years', '10 to 14 years',
'15 to 17 years', '18 and 19 years', '20 years', '21 years', '22 to 24 years',
'25 to 29 years', '30 to 34 years', '35 to 39 years', '40 to 44 years',
'45 to 49 years', '50 to 54 years', '55 to 59 years', '60 and 61 years',
'62 to 64 years', '65 and 66 years', '67 to 69 years', '70 to 74 years',
'75 to 79 years', '80 to 84 years', '85 years and over')

 
TableH_age_labels = c('Under 5 years', '5 to 9 years', '10 to 14 years', 
'15 to 17 years', '18 and 19 years', '20 to 24 years', '25 to 29 years', 
'30 to 34 years', '35 to 44 years', '45 to 54 years', '55 to 64 years', 
'65 to 74 years', '75 to 84 years', '85 years and over')

vars = c( paste0('B01001_00',3:9),  paste0('B01001_0',10:25) ,paste0('B01001_0',27:49), 
          paste0('B01001H_00',3:9), paste0('B01001H_0',10:16),paste0('B01001A_0',18:31))


names(vars) = c( paste0('Total-Male-',Table1_age_labels), 
                 paste0('Total-Female-',Table1_age_labels),
                 paste0('White-Male-',TableH_age_labels), 
                 paste0('White-Female-',TableH_age_labels) ) 


P = get_acs(geography = 'congressional district',
            variables = vars,
            year=2019,
            output = 'tidy') %>% 
      mutate(state_num = as.numeric(substr(GEOID,1,2)),
             dist_num  = as.numeric(substr(GEOID,3,4)))   %>% 
      left_join(fips) %>% 
      transform( age_label = c( Table1_age_labels, 
                                 Table1_age_labels,
                                 TableH_age_labels, 
                                 TableH_age_labels)) %>% 
     left_join(age_group_info)



%>% 
  pivot_wider(id_cols     = GEOID,
              names_from  = variable,
              values_from = estimate) %>% 

## merge info and then aggregate by party

D = left_join(D, P, by=c('state','dist_num'))
