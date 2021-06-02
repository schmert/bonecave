#####################################################
# Creates a series of static frames, each with the
# age-specific fertility rates for a single US
# birth cohort. 
# 
# After running this program I used an external 
# graphics program (GIMP) to create the .gif animation 
# from the separate .png frames. 
#####################################################

library(tidyverse)

rm(list=ls())
graphics.off()

D = read.table(file='USAasfrTRbo.txt',
               skip=2, header=TRUE, na.strings = '.') %>%
    filter( !(Age %in% c('12-','55+'))) %>% 
    mutate(Age = as.numeric(as.character(Age)))
    as_tibble() 

    
coh1920 = D %>% 
          filter(Cohort==1920) %>% 
          group_by(Age) %>% 
          summarize(ASFR = mean(ASFR)) 

coh1965 = D %>% 
    filter(Cohort==1965) %>% 
    group_by(Age) %>% 
    summarize(ASFR = mean(ASFR)) 

    

background_plot = 
  ggplot() +
  aes(x=Age,y=ASFR) +
    geom_line(data=coh1920,color='grey',size=1.5,alpha=.30) +
    geom_line(data=coh1965,color='dodgerblue',size=1.5,alpha=.30) + 
    scale_x_continuous(breaks=seq(15,50,5),
                       limits=c(13,54)) +
    scale_y_continuous(limits=c(-.005,.30)) +
    geom_text(aes(x=45,y=.15),label='1920-borns', color='grey', size=8, alpha=.50) +
    geom_text(aes(x=45,y=.08),label='1965-borns', color='dodgerblue', size=8, alpha=.50) +
    theme_bw() +
    theme(axis.text = element_text(size=13,face='bold'),
          axis.title= element_text(size=15,face='bold'))

print(background_plot)


for (this_cohort in 1920:2000) {

  png(filename=paste0('asfr-',this_cohort,'.png'),
      height=5,width=5,units='in', res=300)
  
  tmp = D %>% 
    filter(Cohort==this_cohort) %>% 
    group_by(Age) %>% 
    summarize(ASFR = mean(ASFR))
  
  this_CFR = sprintf(sum(tmp$ASFR), fmt = '%#.2f')  
  if (max(tmp$Age) < 54) this_CFR = paste0(this_CFR,"+ ?")
  
  cohort_info = paste0('Born in ',this_cohort,'\n',
                       'CFR=',this_CFR)
  
this_plot =
    background_plot +
      geom_text(aes(x=15,y=-.005),label=paste(15+this_cohort),size=3,color='blue') + 
      geom_text(aes(x=25,y=-.005),label=paste(25+this_cohort),size=3,color='blue') +
      geom_text(aes(x=35,y=-.005),label=paste(35+this_cohort),size=3,color='blue') + 
      geom_text(aes(x=45,y=-.005),label=paste(45+this_cohort),size=3,color='blue') +
      geom_point(data=tmp,color='blue') +
      geom_line(data=tmp,color='blue',lwd=0.5) +
      geom_text(aes(x=40,y=.25),label=cohort_info, 
                size=10, color='blue')     
  print(this_plot)

  dev.off()  
}


