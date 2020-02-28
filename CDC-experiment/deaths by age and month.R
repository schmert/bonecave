rm(list=ls())

library(tidyverse)

valid_input_text = head( readLines('Underlying Cause of Death, 1999-2018.txt'), 2892)

age.value = c(0,1,5,15,seq(25,85,10),999)
names(age.value) = c("< 1 year", "1-4 years", "5-14 years", "15-24 years", 
                    "25-34 years", "35-44 years", "45-54 years",  
                    "55-64 years", "65-74 years", "75-84 years", 
                    "85+ years", "Not Stated")

D = read.delim(text=valid_input_text,
               sep='\t', header=TRUE) %>%
             mutate(AgeGroup = factor(Ten.Year.Age.Groups,
                                      levels=names(age.value)),
                    Age = age.value[AgeGroup],
                    mstring = as.character(Month.Code),
                    Year = as.numeric(substr(mstring,1,4)),
                    Month = as.numeric(substr(mstring,6,7)),
                    Date = lubridate::dmy(paste('1',Month,Year))) %>%
             filter(nchar(mstring)==7) %>% 
             select(Date, Year, Month, AgeGroup,
                    Age,Deaths)

ggplot(data=D) +
   aes(x=Date, y=Deaths, group=AgeGroup, color=AgeGroup) +
   scale_x_date() +
   geom_line() +
   facet_wrap(~AgeGroup, ncol=4, scales = 'free')



         