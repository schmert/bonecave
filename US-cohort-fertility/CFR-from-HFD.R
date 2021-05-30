library(tidyverse)

rm(list=ls())

D = read.table(file='USAasfrTRbo.txt',
               skip=2, header=TRUE, na.strings = '.') %>%
    filter( !(Age %in% c('12-','55+'))) %>% 
    as_tibble() %>% 
    group_by(Cohort) %>% 
    mutate(   a   = as.numeric(as.character(Age))) %>% 
    summarize(H     = paste(max(a)),
              L     = paste(min(a)),
              CFR25 = ifelse(L==13 & H>=25,sum(ASFR[a<25])/2,NA),
              CFR30 = ifelse(L==13 & H>=30,sum(ASFR[a<30])/2,NA),
              CFR35 = ifelse(L==13 & H>=35,sum(ASFR[a<35])/2,NA),
              CFR40 = ifelse(L==13 & H>=40,sum(ASFR[a<40])/2,NA),
              CFR45 = ifelse(L==13 & H>=45,sum(ASFR[a<45])/2,NA),
              CFR   = ifelse(L==13 & H==54,sum(ASFR)/2,NA))



ggplot(data=filter(D, Cohort %in% 1920:2000)) +
   geom_text(size=2,aes(x=Cohort, y=CFR, label=H)) +
   theme_bw() +
   scale_y_continuous(limits=range(0,D$CFR,na.rm=TRUE)) +
   geom_text(size=2,aes(x=Cohort, y=CFR40, label='40'), color='red') +
   geom_text(size=2,aes(x=Cohort, y=CFR35, label='35'), color='blue') +
   geom_text(size=2,aes(x=Cohort, y=CFR30, label='30'), color='darkgreen') + 
   geom_text(size=2,aes(x=Cohort, y=CFR25, label='25'), color='purple')  
    
cfr_plot =
    ggplot(data=filter(D, Cohort %in% 1920:2000)) +
        geom_line(size=1.5,aes(x=Cohort, y=CFR)) +
        theme_bw() +
        scale_y_continuous(limits=range(0,D$CFR,na.rm=TRUE),
                           breaks=seq(0,3.5,0.5)) +
        scale_x_continuous(breaks=seq(1920,2000,5),
                           minor_breaks = 1910:2010) +
        geom_point(size=1.5,aes(x=Cohort, y=CFR40), color='red') +
        geom_point(size=1.5,aes(x=Cohort, y=CFR35), color='blue') +
        geom_point(size=1.5,aes(x=Cohort, y=CFR30), color='darkgreen') + 
        geom_point(size=1.5,aes(x=Cohort, y=CFR25), color='purple') +
        geom_vline(xintercept = seq(1920,2000,10),size=0.2) +
        geom_hline(yintercept = 2,size=0.2) +
        labs(title='USA: Average Number of Children Born at Different Ages',
             subtitle='by Woman\'s Year of Birth',
             x = 'Year of Birth',
             y = 'Average # of Children',
             caption = 'Source: Human Fertility Database\nhumanfertility.org 29 May 2021')

print( cfr_plot )

# add small regression lines for last 10 available cohorts,
# extending to next 5

# age 40

reg_width = 1.5
reg_alpha = 0.5

tmp = D %>% 
        filter(is.finite(CFR40)) %>% 
        arrange(Cohort) %>% 
        select(Cohort, CFR40) %>% 
        tail(10)

new = tibble( Cohort= max(tmp$Cohort) + seq(-9,5,1)) 
fit = lm( CFR40 ~ Cohort, data=tmp)

new$CFR40 = predict(fit, new)

cfr_plot = 
    cfr_plot +
    geom_line(data=new, aes(x=Cohort,y=CFR40), color='red',
              size=reg_width, lty=1, alpha=reg_alpha)


# age 35

tmp = D %>% 
    filter(is.finite(CFR35)) %>% 
    arrange(Cohort) %>% 
    select(Cohort, CFR35) %>% 
    tail(10)

new = tibble( Cohort= max(tmp$Cohort) + seq(-9,5,1)) 
fit = lm( CFR35 ~ Cohort, data=tmp)

new$CFR35 = predict(fit, new)

cfr_plot = 
    cfr_plot +
    geom_line(data=new, aes(x=Cohort,y=CFR35), color='blue',
              size=reg_width, lty=1, alpha=reg_alpha)

# age 30

tmp = D %>% 
    filter(is.finite(CFR30)) %>% 
    arrange(Cohort) %>% 
    select(Cohort, CFR30) %>% 
    tail(10)

new = tibble( Cohort= max(tmp$Cohort) + seq(-9,5,1)) 
fit = lm( CFR30 ~ Cohort, data=tmp)

new$CFR30 = predict(fit, new)

cfr_plot = 
    cfr_plot +
    geom_line(data=new, aes(x=Cohort,y=CFR30), color='darkgreen',
              size=reg_width, lty=1, alpha=reg_alpha)


# age 25

tmp = D %>% 
    filter(is.finite(CFR25)) %>% 
    arrange(Cohort) %>% 
    select(Cohort, CFR25) %>% 
    tail(10)

new = tibble( Cohort= max(tmp$Cohort) + seq(-9,5,1)) 
fit = lm( CFR25 ~ Cohort, data=tmp)

new$CFR25 = predict(fit, new)

cfr_plot = 
    cfr_plot +
    geom_line(data=new, aes(x=Cohort,y=CFR25), color='purple',
              size=reg_width, lty=1, alpha=reg_alpha)

# add labels

cfr_plot = cfr_plot +
    geom_text(x=1995, y=0.3, label='Age 25', color='purple') +
    geom_text(x=1990, y=1.0, label='Age 30', color='darkgreen') +
    geom_text(x=1985, y=1.6, label='Age 35', color='blue') +
    geom_text(x=1980, y=2.3, label='Age 40', color='red') +
    geom_text(x=1945, y=3.2, label='Completed (Age 55)', color='black') 
    
    

ggsave(filename='USA-CFR.png', plot=cfr_plot,
       height=6, width=8, dpi=300)

## second version with 2010 observations highlighted

cfr_plot = 
  cfr_plot +
  geom_point(data=filter(D,Cohort+55==2010),
             aes(x=Cohort,y=CFR),
             size=6, color='black',alpha=.50)

hue = c('25'='purple',
        '30'='darkgreen',
        '35'='blue',
        '40'='red')

for (x in c(25,30,35,40)) {
  tmp = filter(D, Cohort + x ==2010)
  cfr_plot = 
    cfr_plot +
    geom_point(data=tmp,
               aes_string(x='Cohort',y=paste0('CFR',x)),
               size=6, color=hue[paste(x)],alpha=.50)
}


ggsave(filename='USA-CFR2.png', plot=cfr_plot,
       height=6, width=8, dpi=300)

