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

ddi    = read_ipums_ddi("idhs_00003.xml")
Women  = read_ipums_micro(ddi)

counts = Women %>% 
           group_by(AGE,CHEB) %>% 
           summarize(n=sum(PERWEIGHT)) %>%
           rename(age=AGE, parity=CHEB)



  G =
    ggplot( data=counts, 
          aes(x=parity, y=age, size=n)) +
       geom_point(shape=16, color='royalblue', alpha=.50) +
       scale_size_continuous(range=c(0,15)) +
       scale_y_continuous(breaks=seq(min_age,50,2)) +
       scale_x_continuous(breaks=0:max(history$parity)) +
       labs(title='Distribution of Women by Age and Parity',
            subtitle='Kenya 2014 DHS') +
       guides(color=FALSE, shape=FALSE,size=FALSE) +
       theme_bw()
  

  print(G)  


