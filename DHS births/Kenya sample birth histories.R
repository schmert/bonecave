####################################################################
# Carl Schmertmann
# created 02 Feb 2019
# altered 02 Feb 2019
#
# construct year-by-year parity histories for a sample of women 
# from the Kenya 2014 DHS (83591 births to 23245 women)
####################################################################

set.seed(6447100)

if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")
library(tidyverse)

# read basic files
ddi    = read_ipums_ddi("idhs_00002.xml")
Births = read_ipums_micro(ddi)

ddi    = read_ipums_ddi("idhs_00003.xml")
Women  = read_ipums_micro(ddi)

# Keep only Kenya 2014
Moms <- Births %>%
          filter(COUNTRY==404, YEAR==2014) %>%
          select(CASEID, 
                 mombirth = BIRTHYEAR,
                 kid      = BIDX,
                 kidbirth = KIDBIRTHYR)

# identify parity-zero women who are not represented
# in the births file

Childless = Women %>% 
             filter(CHEB==0) %>%
             select(CASEID, mombirth = BIRTHYEAR) %>%
             mutate(kid      = NA,
                    kidbirth = NA)

Kenya = rbind(Moms, Childless) %>%
         mutate(woman = as.numeric( as.factor( CASEID)))


# random sample of women
n    = 250
W    = max(Kenya$woman)

selected_women = sample(W, n)

Kenya_sample = Kenya %>%
                 filter(woman %in% selected_women) %>%
                 mutate(m=kidbirth - mombirth)

# lowest year at which any selected mom turned 12
min_age    = 12
first_year = min_age + min(Kenya_sample$mombirth)   # something like 1978
last_year  = max(Kenya_sample$kidbirth, na.rm=TRUE) # almost surely 2014

first_year:last_year

history = data.frame()

for (this_year in first_year:last_year) {

# figure out eacn mom's status in this year

tmp = Kenya_sample %>%
       mutate(year = this_year, 
              age  = this_year - mombirth,
              birth = ifelse( is.na(kidbirth), FALSE, this_year == kidbirth)) %>%
       filter(age >= min_age) %>%
       group_by(woman, year, age,  birth) %>%
       summarize(parity = sum(m <= age, na.rm=TRUE)) %>%
       ungroup() 

 history = rbind(history, 
                 tmp)
}

history = arrange(history, woman, year)

graphics.off()
windows(record=TRUE)

for (this_year in first_year:last_year) {
  
  x = filter(history, year <= this_year)
  G =
    ggplot( data=x, 
          aes(x=parity, y=age, group=as.factor(woman),
              color=as.factor(woman),
              shape=!birth)) +
       geom_point(shape=16, size=1) +
       geom_path() + 
       scale_y_reverse() +
       labs(title=this_year) +
       lims(x= c(0,max(history$parity)),
            y= c(50,min_age)) +
       guides(color=FALSE, shape=FALSE) +
       theme_bw()
  
  G = G + 
       geom_point( data=filter(x,birth), 
                   aes(x=parity, y=age), 
                   size=3, alpha=.50)
  print(G)  
}

