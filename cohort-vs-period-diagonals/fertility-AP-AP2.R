library(tidyverse)
library(HMDHFDplus)

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# make sure that you set the working directory to the
# location of THIS file!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

graphics.off()
rm(list=ls() )

source('fix-HFDparse.R')  # this is necessary for some versions of HFDparse (?)

countries = c('JPN','AUT','FRATNP','HUN','NLD','PRT','IRL')

for (sel_country in countries) {

  
  # Cohort TFR 13-40 from Lexis triangles ----
  
  BTR = readHFD(paste0(sel_country,'birthsTR.txt'))  %>% 
    rename(Births=Total)
  
  NTR = readHFD(paste0(sel_country,'exposTR.txt'))
  
  DTR = full_join(BTR,NTR) 
  
  TFR_triangle = 
        DTR %>% group_by(Cohort,Age) %>% 
               summarize(rate = sum(Births)/sum(Exposure)) %>%
               filter(Age %in% 13:40) %>%
               group_by(Cohort) %>% 
               summarize(TFR=sum(rate), n=n()) %>% 
               filter(n==28) %>% 
               add_column(method='True CFR')
  
  # Cohort TFR from Age-Period rectangles ----
  
  BRR = readHFD(paste0(sel_country,'birthsRR.txt')) %>% 
    rename(Births=Total)
  
  NRR = readHFD(paste0(sel_country,'exposRR.txt'))
  
  DRR = full_join(BRR,NRR) %>% 
       mutate(Cohort = Year-Age,
              rate = Births/Exposure)
  
  TFR_rectangle = 
    DRR %>% 
    filter(Age %in% 13:40) %>%
    group_by(Cohort) %>% 
    summarize(TFR=sum(rate), n=n()) %>% 
    filter(n==28) %>% 
    add_column(method='AP')
  
  # new procedure: average the lagged and leading "rectangle" 
  # measures for each cohort
  
  TFR_new = TFR_rectangle %>% 
    ungroup() %>% 
    mutate(TFR     = (TFR+ lead(TFR,1))/2,
           method='AP2')
  
  df = bind_rows(
          TFR_triangle, 
          TFR_rectangle,
          TFR_new
          )
  
  G = ggplot(data=df) + 
    aes(x=Cohort,y=TFR, shape=method, color=method) + 
    geom_point(size=2, alpha=.70) + 
    geom_line(data=filter(df,method=='AP2')) +
    theme_bw() +
    scale_shape_manual(values=c('square','cross','triangle')) +
    labs(title=paste0(sel_country,' Cohort Fertility over ages 13-40'),
         y='CFR40',
         subtitle='Line = Avg of consecutive A-P squares')
  
  print(G)
  
  ggsave(filename=paste0(sel_country,'-CFR-levels.png'),
         height=8, width=8, units='in')
  
  # calculate errors
  
  err_df = df %>% 
    group_by(Cohort) %>% 
    summarize(e_AP  = 100*(TFR[method=='AP']/TFR[method=='True CFR']-1),
              e_AP2 = 100*(TFR[method=='AP2']/TFR[method=='True CFR']-1)) %>% 
    pivot_longer(cols=starts_with('e_'), 
                 names_to = 'etype', values_to = 'error')
  
  
  G = ggplot(data=err_df) +
    aes(x=Cohort,y=error,color=etype) +
    geom_hline(yintercept = 0) +
    geom_point() +
    geom_line() +
    theme_bw() +
    labs(y='Percent Error', 
         title=sel_country)
  
  print(G)
  
  ggsave(filename=paste0(sel_country,'-CFR-errors.png'),       
         height=8, width=8, units='in')
  
  
  tab = err_df %>% 
    group_by(etype) %>% 
    summarize( MPE   = mean(error,na.rm=TRUE), 
               MAPE  = mean(abs(error),na.rm=TRUE),
               MIN   = quantile(error,prob=0,na.rm=TRUE),
               Q25   = quantile(error,prob=.25,na.rm=TRUE),
               Q75   = quantile(error,prob=.75,na.rm=TRUE),
               MAX   = quantile(error,prob=1,na.rm=TRUE)
               ) 
  
  z = err_df %>% 
       arrange(etype,error) %>% 
       group_by(etype) %>% 
      mutate(P = seq(error)/length(error)) 
  
  G=  ggplot(data=z) +
    aes(x=error,y=P,color=etype) + 
    geom_step() + 
    geom_vline(xintercept = 0) + 
    theme_bw() +
    labs(x='Percent Error', y='Prob', 
         title=paste0(sel_country,' Cumulative Distribution of Errors'))

  print(G)

  ggsave(filename=paste0(sel_country,'-CFR-cumulative-error-distribution.png'),       
         height=8, width=8, units='in')
  
  G=  ggplot(data=err_df) +
    aes(x=error, color=etype) +
    geom_density(trim=TRUE, linewidth=1) +
    geom_vline(xintercept = 0) +
    theme_bw() +
    labs(title=sel_country) +
    scale_x_continuous(limits = c(-2,4))

  print(G)

  ggsave(filename=paste0(sel_country,'-CFR-error-density.png'),       
         height=8, width=8, units='in')
  
} # for sel_country