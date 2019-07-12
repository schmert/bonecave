# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).

library(tidyverse)

## converting from indiv -> households is EXTREMELY slow
## if it's already been done, don't repeat

data.already.processed = TRUE

if (!data.already.processed) {
  if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")
  
  ddi  <- read_ipums_ddi("ipumsi_00015.xml")
  data <- read_ipums_micro(ddi) %>%
            filter(AGE %in% 0:99, 
                   INCEARN < 99999998)
  
  DT = as.data.table(data)
  
  HHsummary = function(INCEARN,AGE,SEX) {
    list(
      inc = sum(INCEARN),
      C   = sum(AGE %in% 0:4)
    )
  }
  
  HHINC = DT[,c('inc','C') := HHsummary(INCEARN,AGE,SEX), by=SERIAL]
  
  
  # ## Household-level data with total earned income and C,W counts
  # MX = data %>%
  #        head(500000) %>%
  #        group_by(SERIAL) %>%
  #        summarize(HHincome = sum(INCEARN[INCEARN < 99999998]),
  #                  C        = sum( AGE %in% 0:4),
  #                  W15      = sum( (SEX==2 & (AGE %in% 15:19))),
  #                  W20      = sum( (SEX==2 & (AGE %in% 20:24))),
  #                  W25      = sum( (SEX==2 & (AGE %in% 25:29))),
  #                  W30      = sum( (SEX==2 & (AGE %in% 30:34))),
  #                  W35      = sum( (SEX==2 & (AGE %in% 35:39))),
  #                  W40      = sum( (SEX==2 & (AGE %in% 40:44))),
  #                  W45      = sum( (SEX==2 & (AGE %in% 45:49))),
  #                  HHWT     = HHWT[1]
  #                  )
  
} # !data.already.processed

