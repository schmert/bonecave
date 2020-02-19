library(HMDHFDplus)
library(tidyverse)
library(showtext)

font_add_google('Source Sans Pro', 'source_sans')

showtext_auto()

theme_carl <- function () { 
  theme_bw(base_size=13, base_family="source_sans") %+replace% 
    theme(
      axis.text        = element_text(size=14, face='bold'),
      legend.text      = element_text(size=12),
      legend.title     = element_text(size=16),
      panel.grid.major = element_line(color=grey(.80),size=0.3),
      panel.grid.minor = element_line(color=grey(.95),size=0.1),
      strip.text       = element_text(size=13, face='bold'),
      plot.title       = element_text(size=25,hjust=0),
      plot.subtitle    = element_text(size=15,hjust=0),
      axis.title       = element_text(size=14)
    )
}

country_list = sort( getHFDcountries() )

graphics.off()

pdf(file='all-available-countries-predicting CFR with TFR at age 25.pdf')

  for (this_country in country_list) {
  
    # un = your HFD username; pw = your HFD password
      
    # cohort x year
    VV = readHFDweb(CNTRY=this_country, item='asfrVV',
                     username=un, password = pw) 
    
    # age x year
    RR = readHFDweb(CNTRY=this_country, item='asfrRR',
                    username=un, password = pw) 
    
    complete_cohorts = VV %>% 
                       group_by(Cohort) %>%
                       summarize(nage = sum(ARDY %in% 15:49)) %>%
                       filter(nage == 35) %>%
                       pull(Cohort)
  
    if (length(complete_cohorts) > 0) {  
      
      y25 = tibble(
              Cohort = complete_cohorts,
              Year   = Cohort + 25
            )
                     
      coh = VV %>%
              filter(Cohort %in% complete_cohorts) %>%
              group_by(Cohort) %>%
              summarize(cfr = sum(ASFR[ARDY %in% 15:49]))
      
      per = RR %>%
              group_by(Year) %>%
              summarize(tfr = sum(ASFR[Age %in% 15:49]))
      
      
      df = per %>%
              inner_join(y25) %>%
              inner_join(coh) %>%
              transform(country = this_country)
  
      if(this_country == country_list[1]) {
          big = df
      } else {
          big =      rbind(big,df)
      }
                  
              
  
      this_span = range(df$tfr, df$cfr)
      
        rho   = round( cor(df$tfr, df$cfr), 2)
        lmfit = lm(cfr~I(tfr-2.1), data=df)
        alpha = round(coef(lmfit)[1],2)
        beta  = round(coef(lmfit)[2],2)
        
        pred = alpha + beta * (df$tfr - 2.1)
        mae  = round( mean( abs( pred - df$cfr)), 2)
        
        this_subtitle = paste0(alpha,' + ', 
                               beta, '*(TFR-2.1) ',
                               ' , Corr= ', rho,
                               ' , Mean Abs Error= ', mae)
      
        G = ggplot(data=df) +
            aes(x=tfr, y=cfr, label=Cohort) +
            scale_x_continuous(limits=this_span) +
            scale_y_continuous(limits=this_span) +
            geom_text(size=4) +
            geom_abline(intercept = 0, slope=1) +
            geom_smooth(method='lm', se=FALSE, lty='dashed',color='red') +
            labs(title    = paste(this_country,'HFD data'),
                 subtitle = this_subtitle,
                 x        = 'TFR when cohort was 25',
                 y        = 'Cohort CFR') +
            theme_carl()
        
        print(G)
        
    } #if
    print(paste(this_country,'has',length(complete_cohorts),
                'complete cohorts'))
  } # for

dev.off()

## summarize the results 

result = big %>%
           group_by(country) %>%
           summarize( ncohorts = n(),
                      rho      = cor(tfr,cfr),
                      alpha    = coef(lm(cfr~I(tfr-2.1)))[1],
                      beta     = coef(lm(cfr~I(tfr-2.1)))[2],
                      MAE      = mean(abs(alpha+beta*tfr - cfr)),
                      )

result %>% arrange(MAE) %>% print(n=99)
