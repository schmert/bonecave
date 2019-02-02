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

ddi   = read_ipums_ddi("idhs_00002.xml")
D     = read_ipums_micro(ddi)
  
Kenya <- D %>%
          filter(COUNTRY==404, YEAR==2014) %>%
          mutate(mom = as.numeric(as.factor(CASEID))) %>%
          select(CASEID, mom, mombirth=BIRTHYEAR, kid=BIDX,kidbirth=KIDBIRTHYR)

# random sample of moms
n    = 100
nmom = max(Kenya$mom)

selected_moms = sample(nmom, n)

Kenya_sample = Kenya %>%
                 filter(mom %in% selected_moms) %>%
                 mutate(m=kidbirth - mombirth)

# lowest year at which any selected mom turned 12
min_age    = 12
first_year = min_age + min(Kenya_sample$mombirth)   # something like 1978
last_year  = max(Kenya_sample$kidbirth)        # almost surely 2014

first_year:last_year

history = data.frame()

for (this_year in first_year:last_year) {

# figure out eacn mom's status in this year

tmp = Kenya_sample %>%
       mutate(year = this_year, 
              age  = this_year - mombirth) %>%
       filter(age >= min_age) %>%
       group_by(mom, year, age) %>%
       summarize(parity = sum(m <= age)) %>%
       ungroup() 

 history = rbind(history, 
                 tmp)
}

graphics.off()
windows(record=TRUE)

for (this_year in first_year:last_year) {
  
  G =
    ggplot( data=filter(history, year==this_year), 
          aes(x=parity, y=age, group=mom)) +
       geom_point(alpha=.40) +
       ylim(min_age,50) +
       xlim(c(0, max(history$parity))) +
       scale_y_reverse() +
       labs(title=this_year) +
       theme_bw()
    
  print(G)  
}

