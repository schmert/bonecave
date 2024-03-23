library(tidyverse)
library(here)  # points to /bonecave

death_data = here('USA-deaths-from-HCD','USA_d_interm_idr_orig.csv')
expos_data = here('USA-deaths-from-HCD','USA_e.csv')

# read death data and keep drug abuse deaths

D = read_csv(death_data) %>% 
     filter(cause=='28',sex %in% 1:2) %>% 
     mutate(sex=factor(sex,levels=1:2,labels=c('Male','Female'))) %>% 
     pivot_longer(cols=starts_with('d'),
                  values_to = 'deaths',
                  names_to  = 'agecat')

# use 90 as the start of the open age interval,
# change the agecat variable to numeric
# select vars of interest

keepers = paste0('d',c(0,1,seq(5,85,5),'90p'))

D = D %>% 
     filter(agecat %in% keepers) %>% 
     transform(age=c(0,1,seq(5,85,5),90)) %>% 
     select(year,sex,cause,age,deaths)
     

# read exposure data

N = read_csv(expos_data) %>% 
     filter(sex %in% 1:2) %>% 
     mutate(sex=factor(sex,levels=1:2,labels=c('Male','Female'))) %>% 
     pivot_longer(cols=starts_with('e'),
                  values_to='expos',
                  names_to='agecat')

# use 90 as the start of the open age interval,
# change the agecat variable to numeric
# select vars of interest

keepers = paste0('e',c(0,1,seq(5,85,5),'90p'))

N = N %>% 
  filter(agecat %in% keepers) %>% 
  transform(age=c(0,1,seq(5,85,5),90)) %>% 
  select(year,sex,age,expos)

# merge the death and exposure data

df = inner_join(D,N,by=c('year','sex','age')) %>% 
        mutate(agegroup = cut(age,breaks=c(seq(0,90,30),Inf),right=FALSE)) %>% 
        group_by(year,sex,agegroup) %>% 
        summarize(rate = 100000*sum(deaths)/sum(expos))

# it looks like there is some disconinuity
# in the time series starting
# in 2007 (new cause defns?), so
# only plot from 2008 on

keepers = c('[30,60)','[60,90)')
keepernames = c('Age 30-59','Age 60-89')

tmp = df %>% 
       filter(year > 2007,
              agegroup %in% keepers) %>% 
       mutate(agegroup=factor(agegroup,
                              levels=keepers,
                              labels=keepernames))

G = ggplot(data=tmp) +
   aes(x=year,y=rate,color=factor(sex)) +
   geom_point(size=2) +
   geom_smooth(se=FALSE,lwd=0.8, span=.90) +
   facet_wrap(~agegroup) +
   scale_y_continuous(limits=c(0,3)) +
   scale_x_continuous(breaks=seq(2008,2020,2)) +
   scale_color_manual(values=c('royalblue','firebrick')) +
   guides(color='none') +
   theme_bw() +
   labs(x='Year',y='rate per 100,000',
        title='US Drug Abuse Death Rates, by sex, 2008-2019',
        caption='Source: Human Cause of Death Data Series, https://mortality.org\n@CSchmert')

text_df = tmp %>% 
            filter(year==2010) 

G = G +
     geom_text(data=text_df,
               aes(label=sex), nudge_y = c(+.60,+.60,-.25,-.25))

ggsave(filename=here('USA-deaths-from-HCD','US-drug-abuse-death-rates-by-age.png'),
       plot=G, height=6, width=6, dpi=300)
