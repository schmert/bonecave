#####################################################################
# Question: what is the fraction of US-born, married (spouse present)
# people who have a spouse born in a different state or country?
#
# This might (?) serve as a useful indicator of population mobility
# and migration.
#
# Input data is from IPUMS, 1880-2000 decennial censuses and 2015 ACS
# downloaded 22 Nov 2019 via an extract request from http://ipums.org
#####################################################################
library(tidyverse)
library(ipumsr)

if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

ddi  <- read_ipums_ddi("usa_00085.xml")

# keep only those who are married, spouse present
data <- read_ipums_micro(ddi) %>%
         filter(MARST==1) %>%
         select(YEAR,SERIAL,STATEFIP,PERNUM,PERWT,SPLOC,BPL) %>%
         mutate_if(is.labelled, zap_labels)


# converts the person-level information 
# for married, spouse-present individuals within
# a single household into a vector, with counts
# of how many US-born, married-spouse-present
# individuals have spouses with the same birthplace,
# and how many have spouses with difft birthplaces

make_couples = function(pernum,perwt,sploc,bpl) {
   usborn = bpl %in% c(1:56,100:120)
   i        = seq(pernum)         # 1,2,..,2n (n=# of couples in HH)
   imatch   = match(pernum,sploc) # spouse location among 1,2,...,2n
   
   i1 = unique( pmin(i,imatch))  # indices of 'main' person for each couple
   i2 = imatch[i1]               # indices of 'partner' for each couple

try(     
  data.frame(b1=bpl[i1],
             b2=bpl[i2],
             usborn = sapply(1:length(i1), function(j) sum(usborn[j], usborn[imatch[j]])),
             wt     = perwt[i1])
    )
}

tmp = data %>%
        group_by(YEAR,SERIAL) %>%
        summarize( couples=list(make_couples(PERNUM,PERWT,SPLOC,BPL))) %>%
        unnest() %>%
        mutate(same_bpl = (b1==b2))




   
tmp2 = tmp %>%
         group_by(YEAR) %>%
         summarize( total = sum(wt,na.rm=TRUE),
                    nb0       = 100*sum(wt[usborn==0],na.rm=TRUE)/total,
                    nb1       = 100*sum(wt[usborn==1],na.rm=TRUE)/total,
                    nb2_diff  = 100*sum(wt[usborn==2 & !same_bpl],na.rm=TRUE)/total,
                    nb2_same  = 100*sum(wt[usborn==2 &  same_bpl],na.rm=TRUE)/total
                    ) %>%
         select(-total) %>%
         gather(-YEAR, key='type', value='pct')
     
theme_carl <- function () { 
   theme_bw(base_size=12) %+replace% 
      theme(
         title      = element_text(size=16, face='bold', hjust =0.5),
         axis.text  = element_text(size=11, face='bold'),
         axis.title = element_text(size=13, face='bold')
      )
}

label_df = 
   data.frame(   
      type = c('nb0','nb1','nb2_diff','nb2_same'),   
      YEAR = rep(2000, 4),
      pct  = filter(tmp2,YEAR==2000)$pct +
                    c(+5.5,-4,+5,+6),
      desc = c('Both Spouses Foreign-Born',
               'One Spouse Foreign-Born',
               'Both Spouses US Natives\nof Difft States',
               'Both Spouses US Natives\nof Same State')
   )

ggplot(data=tmp2) +
   aes(x=YEAR, y=pct, color=type) +
   geom_point(size=2.5) +
   geom_line(lwd=1.5) +
   scale_y_continuous(limits=range(0,tmp2$pct),
                      breaks=seq(0,50,10),
                      minor_breaks = NULL) +
   scale_x_continuous(breaks=seq(1880,2020,10),
                      minor_breaks = NULL) +
   guides(color=FALSE) +
   geom_text(data=label_df, 
             aes(x=YEAR,y=pct,color=type,label=desc), 
                 size=4, fontface='bold',inherit.aes = FALSE) +
   labs(title='Percent of US Married Couples by Nativity, 1880-2015',
        caption='Source: http://ipums.org\nCensus samples for 1880-2000, ACS for 2015',
        y='Percent of All Couples') +
   theme_carl()

ggsave(file='mixed-marriage.png', width=8, height=8,
       units='in', dpi=300)



