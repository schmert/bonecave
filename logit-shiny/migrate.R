# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).

library(tidyverse)

if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

ddi  <- read_ipums_ddi("usa_00079.xml")
data <- read_ipums_micro(ddi) %>%
         filter(AGE %in% 20:59) %>%
         mutate( mover = 1*(MIGRATE1 != 1),
                 male  = 1*(SEX==1)) %>%
         select(age=AGE, male, mover, wt=PERWT)

## create a fake sample of 500 men, 500 women
## by drawing from data with probs prop to wts

sel = sample(x=nrow(data), size=1000, prob=data$wt, replace=TRUE)

simdata = data[sel,] %>%
           select(-wt)

ggplot(data=simdata, aes(x=age, y=mover, color=factor(male))) +
    geom_jitter(height=.04) +
    geom_smooth()




