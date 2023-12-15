## -----------------------------------------------------------
##  The dangers of drawing cohort profiles from period data 
##
##  EXAMPLE 1 
##  Cohort summary measures from period age-specific rates
##
##  Authors:      blind submission
##  Last update:  15/09/2021
##
##  sessionInfo() details:
##  
##  R version 4.0.2 (2020-06-22)
##  Platform: x86_64-apple-darwin17.0 (64-bit)
##  Running under: macOS Mojave 10.14.6
##  
##  locale: en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
##
##  attached base packages:
##  stats  graphics  grDevices  utils  datasets 
##  methods  base     
## 
##  other attached packages:
##  patchwork_1.1.1       HMDHFDplus_1.9.13         
##  forcats_0.5.0         stringr_1.4.0         
##  dplyr_1.0.5           purrr_0.3.4           readr_1.3.1          
##  tidyr_1.1.0           tibble_3.0.2         
##  ggplot2_3.3.2         tidyverse_1.3.0
## -----------------------------------------------------------

## cleaning the workspace
#rm(list=ls())

## set up the directory where .R is saved (R-studio command)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## loading useful packages
library(tidyverse)
library(HMDHFDplus)     ## for loading data from HMD and HFD
library(patchwork)      ## for inset plots

##--- FERTILITY EXAMPLE ------------

## set your username and password 
myusername <- un
mypassword <- pw

## select country of interest
cou <- "JPN"

## reading cohort fertility summary measure of interest
df.fert_coh <- readHFDweb(CNTRY = cou, item = "tfrVH",
                          username = myusername,password = mypassword)
head(df.fert_coh)
tail(df.fert_coh)

## let us focus on completed cohort fertility by age 40 
## to have a longer time series
ccf40 <- df.fert_coh$CCF40
cohorts <- df.fert_coh$Cohort
m <- length(cohorts)
plot(cohorts,ccf40)

## let us now compute the same information from period data

## downloading period  data (age-specific fertility rates)
df.fert_per <- readHFDweb(CNTRY = cou, item = "asfrRR",
                          username = myusername,password = mypassword)
head(df.fert_per)
tail(df.fert_per)

## ages and years of interest
ages <- min(df.fert_per$Age):39  ## ccf40 is obtained by summing until age 39
years <- min(df.fert_per$Year):max(df.fert_per$Year)

## matrix of age-specific fertility rates (period)
ASFR <- matrix(df.fert_per[df.fert_per$Age%in%ages,]$ASFR, nrow=length(ages)) 
rownames(ASFR) <- ages
colnames(ASFR) <- years

## computing ccf40 from diagonals of period data

## first cohort available from period data
c1 <- years[1] - ages[1]
## last cohort available from period data
c2 <- years[length(years)] - ages[length(ages)]
cohorts_period <- c1:c2
mp <- length(cohorts_period)
ccf40_per <- rep(NA,mp)

## extract diagonal and compute ccf40
i <- 1
for (i in 1:mp){
  ASFRsub <- ASFR[,1:length(ages)+i-1]
  ccf40_per[i] <- sum(diag(ASFRsub))
}

## plotting the two measures
plot(cohorts,ccf40)
points(cohorts_period,ccf40_per,pch=4,col=2)

## save and keep only relevant results
my.results_coh <- tibble(cohort = cohorts,ccf40 = ccf40)
my.results_per <- tibble(cohort = cohorts_period,ccf40_per = ccf40_per)
my.results_fer <- my.results_coh %>% 
  left_join(my.results_per, by = "cohort") %>% 
  drop_na()
rm(list=setdiff(ls(),"my.results_fer"))

my.results_fer %>% 
  pivot_longer(cols=c(-cohort)) %>% 
  ggplot(aes(x=cohort,y=value,color=name,shape=name)) +
  geom_point(size=2) 


##--- MORTALITY EXAMPLE ------------

## load lifetable functions (to compute e0)
source("LifeTableFunctions.R")

## set your username and password 
myusername <- "XXXXXXXXXX" 
mypassword <- "YYYYYYYYYY"

## select country of interest
cou <- "SWE"

## loading age-specific cohort mortality rates from HMD
swe_cohort <- readHMDweb(CNTRY = cou, item = "cMx_1x1", 
                         fixup = TRUE,
                         username = myusername,password = mypassword)

## extract ages, cohorts and female mortality rates
ages <- unique(swe_cohort$Age)
cohorts <- unique(swe_cohort$Year)
m <- length(ages)
n <- length(cohorts)
cMX_full <- matrix(swe_cohort$Female,m,n)
rownames(cMX_full) <- ages
colnames(cMX_full) <- cohorts

## keep only completed cohorts

## last cohort with full data
## (look at the matrix due to NAs at older ages in previous cohorts)
c2 <- 1908

## first cohort with full data
c1 <- cohorts[which(!is.na(cMX_full[1,]))[1]]

## start from later cohort to have same length as 
## plot for fertility example
c1 <- c2 - nrow(my.results_fer) + 1

## subset matrix
cMX <- cMX_full[,cohorts%in%(c1:c2)]

## new cohorts
cohorts <- c1:c2
n <- length(cohorts)

## loading age-specific period mortality rates from HMD
swe_period <- readHMDweb(CNTRY = cou, item = "Mx_1x1", 
                         fixup = TRUE,
                         username = myusername,password = mypassword)

## extract years and female mortality rates
years <- unique(swe_period$Year)
ny <- length(years)
MX_full <- matrix(swe_period$Female,m,ny)
rownames(MX_full) <- ages
colnames(MX_full) <- years

## subset period matrix (start from first cohort)
MX <- MX_full[,years>=c1]

## create cohort matrix from diagonal of period rates
cMX_per <- matrix(NA,m,n)
i <- 1
for (i in 1:n){
  MXsub <- MX[,1:m+i-1]
  cMX_per[,i] <- diag(MXsub)
}
rownames(cMX_per) <- ages
colnames(cMX_per) <- cohorts

## compute life expectancy at birth
e0c <- e0c_per <- rep(NA,n)
i <- 1
for (i in 1:n){
  lt1 <- lifetable.mx(x=ages,mx = cMX[,i],sex = "F")
  lt2 <- lifetable.mx(x=ages,mx = cMX_per[,i],sex = "F")
  e0c[i] <- lt1$ex[1]
  e0c_per[i] <- lt2$ex[1]
}

## plotting the two measures
plot(cohorts,e0c)
points(cohorts,e0c_per,pch=4,col=2,lwd=2)

## save and keep only relevant results
my.results_mor <- tibble(cohort = cohorts,e0c = e0c,
                         e0c_per=e0c_per)
rm(list=setdiff(ls(),c("my.results_fer","my.results_mor")))

## FINAL PLOT

## prepare for plot
my.df1 <- my.results_fer %>% 
  rename("Cohort measure"=ccf40,
         "From period data"=ccf40_per) %>% 
  pivot_longer(cols=c(-cohort)) %>% 
  mutate(title="Completed cohort fertility by age 40 (Japan)")

my.df2 <- my.results_mor %>% 
  rename("Cohort measure"=e0c,
         "From period data"=e0c_per) %>% 
  pivot_longer(cols=c(-cohort)) %>% 
  mutate(title="Life expectancy at birth (Sweden)")

my.df <- my.df1 %>% 
  bind_rows(my.df2)

## main plot
f1 <- my.df %>% ggplot(aes(x=cohort,y=value,color=name,shape=name)) +
  geom_point(size=2.5,stroke=1.5) +
  facet_wrap(.~title,scales = "free") +
  theme_bw(base_size = 18) +
  labs(x="Cohort",y=NULL) +
  scale_x_continuous(breaks = seq(1865,2105,10)) +
  scale_color_manual(values = c(1,4)) +
  scale_shape_manual(values=c(1, 4)) +
  theme(legend.title = element_blank(),
        # legend.position = c(.7, .9),
        legend.position = c(.85, .15),
        legend.text=element_text(size=20))

f1

## preparing inset plots for percentage difference
inset1 <- my.results_fer %>% 
  mutate(reldiff=(ccf40_per-ccf40)/ccf40)
inset2 <- my.results_mor %>% 
  mutate(reldiff=(e0c_per-e0c)/e0c)

f2 <- inset1 %>% ggplot(aes(x=cohort,y=reldiff)) +
  geom_line() +
  theme_bw(base_size = 14) +
  labs(x=NULL,y=NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = seq(-0.04,0.04,0.02),
                     limits = c(-0.019,0.041)) +
  scale_x_continuous(breaks = seq(1865,2105,10)) +
  ggtitle("Percentage difference") +
  theme(plot.title=element_text(size=12))
f2

f3 <- inset2 %>% ggplot(aes(x=cohort,y=reldiff)) +
  geom_line() +
  theme_bw(base_size = 14) +
  labs(x=NULL,y=NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = seq(-0.04,0.04,0.02),
                     limits = c(-0.019,0.041)) +
  scale_x_continuous(breaks = seq(1865,2105,10)) +
  ggtitle("Percentage difference") +
  theme(plot.title=element_text(size=12))
f3
  
## final plot
f1 + 
  inset_element(f2, left = 0.005, 
                   right = 0.28,
                   bottom = 0.05, 
                   top = 0.35) +
  inset_element(f3, left = 0.55, 
                right = 0.825,
                bottom = 0.65, 
                top = 0.95)

## END

