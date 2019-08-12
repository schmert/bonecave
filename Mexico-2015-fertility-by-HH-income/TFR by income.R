# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).

library(tidyverse)
library(data.table)

if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

ddi  = read_ipums_ddi("ipumsi_00015.xml")
data = read_ipums_micro(ddi) %>%
          mutate(inc = ifelse(INCEARN < 99999998, INCEARN,0),
                 C   = AGE < 5,
                 W15 = (SEX==2 & AGE %in% 15:19),
                 W20 = (SEX==2 & AGE %in% 20:24),
                 W25 = (SEX==2 & AGE %in% 25:29),
                 W30 = (SEX==2 & AGE %in% 30:34),
                 W35 = (SEX==2 & AGE %in% 35:39),
                 W40 = (SEX==2 & AGE %in% 40:44),
                 W45 = (SEX==2 & AGE %in% 45:49))

#--------------------------------------
## conversion from indiv -> HH level is MUCH MUCH MUCH MUCH faster 
## with data.table than tidyverse, so switch temporarily to data.table

      DT = as.data.table(data)
      
      HHINC = DT[, sum(inc) , by=SERIAL]
      C0    = DT[, sum(C)   , by=SERIAL]
      W15   = DT[, sum(W15) , by=SERIAL]
      W20   = DT[, sum(W20) , by=SERIAL]
      W25   = DT[, sum(W25) , by=SERIAL]
      W30   = DT[, sum(W30) , by=SERIAL]
      W35   = DT[, sum(W35) , by=SERIAL]
      W40   = DT[, sum(W40) , by=SERIAL]
      W45   = DT[, sum(W45) , by=SERIAL]
      HHWT  = DT[, HHWT[1]  , by=SERIAL]

#--------------------------------------
    
## convert data.table summaries back to a standard data.frame
df = data.frame(
        hhid   = HHINC$SERIAL,
        income = cut( HHINC$V1, breaks=c( seq(0,40000,5000),Inf), right=FALSE),
        C      = C0$V1,
        W15    = W15$V1,
        W20    = W20$V1,
        W25    = W25$V1,
        W30    = W30$V1,
        W35    = W35$V1,
        W40    = W40$V1,
        W45    = W45$V1,
        wt     = HHWT$V1 / mean(HHWT$V1)
) %>%
  group_by(income) %>% 
  summarize( C = sum(wt*C),
             W15 = sum(wt*W15),
             W20 = sum(wt*W20),
             W25 = sum(wt*W25),
             W30 = sum(wt*W30),
             W35 = sum(wt*W35),
             W40 = sum(wt*W40),
             W45 = sum(wt*W45)
             ) %>%
  mutate( W      = W15+W20+W25+W30+W35+W40+W45,
          iTFR   = 7 * C/W,
          pi2534 = (W25+W30)/W,
          xTFR   = (10.65 - 12.55 * pi2534) * C/W) 
      
levels(df$income) = c('0-5K','5-10K','10-15K','15-20K',
                      '20-25K','25-30K','30-35K','35-40K','> 40K')

#------------------------
# superfast bTFR
#------------------------

source('superfast_bTFR function.R')

C      = round(df$C)
W      = as.matrix( select(df, paste0('W',seq(15,45,5) ) ))
q5_est = rep(.020, nrow(df))

result        = superfast_bTFR(C,W,q5_est, delta_TFR=.01,
                               rownames=levels(df$income))


for (k in names(result)) {
  plot( result[[k]]$dens, xlim=c(1,3), main=k)
  abline(v=result[[k]][c('Q10','Q50','Q90')])
}

result_df = data.frame(
              income = df$income,
              Q10   = unlist( sapply(result,'[','Q10') ),
              Q50   = unlist( sapply(result,'[','Q50') ),
              Q90   = unlist( sapply(result,'[','Q90') )
            )
              
#------------------------

## plot TFR by HH income  
ggplot(data=result_df, aes(x=income, y=Q50, group=NA)) +
  geom_point(color='darkgreen', size=3) +
  geom_line(color='darkgreen', lwd=1.5) +
  geom_hline(yintercept=2, lwd=0.5) +
  geom_ribbon(aes(x=income,ymin=Q10,ymax=Q90), fill='darkgreen', alpha=.20) +
  labs(title='Total Fertility by Household Income, Mexico 2011-2015',
       subtitle='posterior median and 80% interval',
       x='Earned Income (pesos/month)',
       y='Total Fertility (lifetime children/woman)',
       caption='Source: 2015 Mexican Intercensal Survey, http://international.ipums.org') +
  theme_bw() +
  theme(axis.text  = element_text(face='bold', size=11),
        axis.title = element_text(face='bold', size=12))

ggsave(file='MEX 2015 TFR by HH income.png', 
       height=8, width=11.5, units='in', dpi=400)

