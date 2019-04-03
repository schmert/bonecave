#########################################################################
# Carl Schmertmann
# 3 Apr 2019
#
# Age/Sex distribution of all counties in a chosen US state
# ACS 2012-2016 data
# using 'acs' and 'ggplot' packages
#########################################################################

library(acs)
library(tidyverse)

my_state     = 'FL'
acs_end_year = 2017 

# these colors are supposedly colorblind-friendly and printable
female_color = 'gold2'
male_color   = 'indianred'   

## You must have your own api key: get it from
## https://api.census.gov/data/key_signup.html 
## 
## Once you have a key, just change the command below 
## to something like
## api.key.install( 'hfdsjku978dfsks...' ) with your own key in quotes

api.key.install( scan('api_key.txt',what='character') )  


## set up the 'geo.set' object that the ACS package uses to select  
## counties in your state as the geography of interest
## Note that you could change county= to other political geographies

G = geo.make(state=my_state, county = c('Charlotte','Leon','Sumter','Marion'))

## fetch the age-sex distribution of each location 
## (table B01001, according to the documentation at 
## http://www2.census.gov/programs-surveys/acs/summary_file/2012/documentation/5_year/user_tools/ACS2012_5-Year_TableShells.xls )

A =  acs.fetch( endyear=acs_end_year, span=5, 
                geography=G, table.number = 'B01001',
                col.names='pretty')

## keep only the estimates
A = A@estimate

## put counties in alphabetical order
A = A[ order(rownames(A)), ]

## the data in A includes some double-counting of population, and several five-year 
## age groups are split. 

# for each of column of A, identify which sex/five-year age group it belongs to (NA = none)
group = c( rep(NA,2), 'M00','M05','M10',rep('M15',2),rep('M20',3),'M25','M30','M35','M40','M45',
           'M50','M55', rep('M60',2), rep('M65',2), 'M70','M75','M80','M85',
           
           NA, 'F00','F05','F10',rep('F15',2),rep('F20',3),'F25','F30','F35','F40','F45',
           'F50','F55', rep('F60',2), rep('F65',2), 'F70','F75','F80','F85')

age_group_label = c('0-4','5-9','10-14','15-19','20-24','25-29',
                    '30-34','35-39','40-44','45-49','50-54','55-59',
                    '60-64','65-69','70-74','75-79','80-84','85+')

# aggregate populations into sex x five-year age groups for each row of A           
pop = t( apply(A, 1, function(x) tapply(x,group,sum)) )

# plot the age pyramids

# convert the population data from matrix -> (long) data.frame

state_data = expand.grid( loc=rownames(pop), 
                          age=seq(0,85,5), 
                          sex=c('Female','Male'),
                  stringsAsFactors = FALSE)

# add the population as a final column
state_data$pop = as.vector(pop)

# change the sign of the male pops
state_data = mutate(state_data,  pop = pop * ifelse(sex=='Male', -1, +1))



locations = unique(state_data$loc)

# reset graphics 
graphics.off()     

png(file=paste0('FL-age-pyramids-', my_state ,'.png'),
    width=10, height=10, units='in', res=600)  # open 


  ## draw the age pyramid for each location:
  ## this is based on Kyle Walker's code
  

    pyramid = ggplot(state_data,
                     aes(x = age, y = pop, fill = sex)) + 
      geom_bar(data=filter(state_data,sex == "Female"), stat = "identity")  + 
      geom_bar(data=filter(state_data,sex == "Male")  , stat = "identity") + 
      scale_y_continuous(labels = abs) +
      scale_x_continuous(breaks=seq(0,85,10),
                         labels=age_group_label[seq(1,17,2)]) +
      labs(y='Population', x='Age Group',
           fill='',
           caption=paste0('Source: American Community Survey ',
                         acs_end_year-4,'-',acs_end_year)) +
      coord_flip() + 
      scale_fill_manual(values=c(female_color, male_color)) +
      theme_bw() +
      theme(strip.text=element_text(size=12,face='bold')) +
      facet_wrap(~loc,nrow=2,ncol=2) 
    
    print( pyramid )
  

dev.off()  # close png