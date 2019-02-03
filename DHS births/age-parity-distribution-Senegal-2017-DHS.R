####################################################################
# Carl Schmertmann
# created 02 Feb 2019
# altered 03 Feb 2019
####################################################################

rm(list=ls())
set.seed(6447100)

if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")
library(tidyverse)

ddi    = read_ipums_ddi("idhs_00003.xml")
Women  = read_ipums_micro(ddi) %>%
           filter(COUNTRY==686,
                  YEAR==2017) %>%
           select(CASEID, BIRTHYEAR, AGE, CHEB, PERWEIGHT)

counts = Women %>% 
           group_by(AGE,CHEB) %>% 
           summarize(n=sum(PERWEIGHT)) %>%
           rename(age=AGE, parity=CHEB)


## construct the basic 'bubble plot' of the
## age x parity distribution



  G =
    ggplot( data=counts, 
          aes(x=parity, y=age, size=n)) +
       geom_point(shape=16, color='orangered', alpha=.50) +
       scale_size_continuous(range=c(0,15)) +
       scale_y_continuous(breaks=seq(15,49,2)) +
       scale_x_continuous(breaks=0:max(counts$parity)) +
       labs(title='Distribution of Women by Age and Parity',
            subtitle='Senegal 2017 DHS',
            x='# children ever born',
            caption='Source: IPUMS-DHS, https://www.idhsdata.org/idhs/') +
       guides(color=FALSE, shape=FALSE,size=FALSE) +
       theme_bw()
  

  print(G)  
  
## construct histories for a few women (this uses
## the information in the births file)

  ddi    = read_ipums_ddi("idhs_00002.xml")
  Births = read_ipums_micro(ddi)

# identify CASEIDs for some women at specific ages and parities
  sel = data.frame(
    age   = c( 40, 40, 35, 46),
    parity= c(  1,  6,  3, 14 )
  )
  selected_women = NULL
  
  for (i in 1:nrow(sel)) {
    ix = which( (Women$AGE == sel$age[i]) &
                (Women$CHEB == sel$parity[i]) )
    elig = as.character(ix)  # just in case it's a single integer
    chosen = as.numeric( sample(elig,1) )
    selected_women = c(selected_women, as.numeric(chosen))    
  }
  
  selected_CASEID = Women$CASEID[selected_women]
  
# only keep births to the small # of selected women  
  Small = Births %>%
            filter(CASEID %in% selected_CASEID) %>%
            select(CASEID, KIDBIRTHYR) %>%
            left_join(Women, by='CASEID') %>%
            mutate( m = KIDBIRTHYR - BIRTHYEAR,
                    woman_id = as.numeric(as.factor(CASEID))) %>%
            select(woman_id, agenow=AGE, m, cheb=CHEB, kidbirth=KIDBIRTHYR)
  
# figure out age, parity for each selected woman, from age=15 to current age  
  
  history = data.frame()
  
  for (this_id in unique(Small$woman_id)) {
    for (this_age in 15:Small$agenow[this_id]) {
      mvec = Small$m[Small$woman_id == this_id]
      tmp = data.frame( woman_id = this_id,
                        age      = this_age,
                        parity   = sum( mvec <= this_age))
      
      history = rbind(history,
                      tmp)
      
    }
  }
      
# add the paths to the diagram
  
  G + geom_point(data=history,
                 aes(x=parity, y=age, group=woman_id), size=2,
                 inherit.aes = FALSE) +
      geom_line(data=history,aes(x=parity, y=age, group=woman_id,
                color=woman_id),
                inherit.aes = FALSE)
    

## save the plot    
# png(file='age-parity-distribution-Senegal-2017-DHS.png')
#   
# dev.off()


