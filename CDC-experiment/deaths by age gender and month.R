###############################################
# CDC WONDER experiment
#
# Carl Schmertmann 07 Nov 2019
###############################################

# 07 Nov 2019: did not work with build_vignettes = TRUE 
# error msg was 
# Error: Failed to install 'wonderapi' from GitHub:
#   System command error, exit status: 1, stdout + stderr (last 10 lines):
#   E> 
#   E>  Attempting to reconstruct a stack trace...
# E> 
#   E>    Frame	Code address
# E>  * 0x22daf0	0x3f7ec3a C:\PROGRA~1\RStudio\bin\pandoc\pandoc.exe+0x3b7ec3a
# E>  * 0x22daf8	0x231790
# E> 
#   E> Error: processing vignette 'D76codebook.Rmd' failed with diagnostics:
#   E> pandoc document conversion failed with error 11
# E> Execution halted

# devtools::install_github("socdataR/wonderapi", build_vignettes = TRUE)

## installation was VERY finicky. It only worked with 
## build_vignettes=FALSE, and only after updating ALL packages
## before trying to install

library(tidyverse)

if (!require(wonderapi)) {
  devtools::install_github("socdataR/wonderapi", build_vignettes = FALSE)
}

show_databases()

mydata0 = getData(agree=TRUE, db='Detailed Mortality')
                  

@@@ NOT WORKING @@@@ # queries aren't valid

Qlist = list( 
             list( 'Group Results By', 'Ten-Year Age Groups')
            )

Qlist = list( 
  list('Group Results By', 'Weekday'),
  list('And By', 'Year')
)

mydata0 = getData(agree=TRUE, db='Detailed Mortality', 
                  querylist=Qlist)


# first experiment: plot state- and gender- specific CDR

Qlist = list( list( 'Group Results By', 'Month'))

mydata1 = getData(agree=TRUE, db='Detailed Mortality', 
                  querylist=Qlist) %>%
  filter( !(Weekday == 'Unknown'))

ggplot(data=mydata1) +
  aes(x=Year, y=Deaths, color=Weekday) +
  geom_point() +
  geom_line() +
  theme_bw()





