library(tidyverse)

# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).

if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

ddi  <- read_ipums_ddi("usa_00090.xml")
data <- read_ipums_micro(ddi) %>% 
         mutate_if(is.labelled, zap_labels) %>% 
         mutate(YEAR = ifelse(YEAR!=2019, YEAR, 2020))


z = data %>% 
     filter(YEAR >= 1960, AGE > 25) %>% 
     mutate(COHORT = YEAR - AGE) %>% 
     group_by(YEAR, AGE, COHORT) %>% 
     summarize(frac_own = weighted.mean(OWNERSHP==1, PERWT)) 

ggplot(data=filter(z, COHORT %in% c(1940,1950,1960,1970, 1980))) +
  aes(x=AGE,y=frac_own,color=factor(COHORT)) + 
  geom_point() + 
  geom_line() +
  scale_y_continuous(limits=range(.40,z$frac_own)) +
  theme_bw()

