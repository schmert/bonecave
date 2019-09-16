rm(list=ls())

library(tidyverse)
library(wpp2019)

data("popMprojMed")
data("popFprojMed")

toss = c(
"World", 
"Sub-Saharan Africa",
"Northern Africa and Western Asia", 
"Central and Southern Asia", 
"Eastern and South-Eastern Asia", 
"Latin America and the Caribbean", 
"Australia/New Zealand", 
"Oceania (excluding Australia and New Zealand)", 
"Europe and Northern America", 
"Africa", 
"Asia", 
"Europe", 
"Northern America", 
"Oceania", 
"More developed regions", 
"Less developed regions", 
"Least developed countries", 
"Other less developed countries", 
"Less developed regions, excluding China", 
"Land-locked Developing Countries (LLDC)", 
"Small Island Developing States (SIDS)", 
"High-income countries", 
"Middle-income countries", 
"Upper-middle-income countries", 
"Lower-middle-income countries", 
"Low-income countries", 
"No income group available", 
"Eastern Africa", 
"Middle Africa", 
"Southern Africa", 
"Western Africa", 
"Northern Africa", 
"Western Asia", 
"Central Asia", 
"Southern Asia", 
"Eastern Asia", 
"South-Eastern Asia", 
"Caribbean", 
"Central America", 
"South America", 
"Eastern Europe", 
"Northern Europe", 
"Southern Europe",
"Western Europe",
"Melanesia",
"Micronesia",
"Polynesia")

old_ages = c( paste0(seq(75,95,5),
                     '-',
                     seq(79,99,5)),
              '100+')

Male = popMprojMed %>%
         filter(!(name %in% toss)) %>%
         select(country_code:age, M2025=`2025`, M2100=`2100`) 

Female = popFprojMed %>%
          filter(!(name %in% toss)) %>%
          select(country_code:age, F2025=`2025`, F2100=`2100`) 

pop = cbind(Male, select(Female, contains('F'))) %>%
         mutate(pop2025 = M2025 + F2025,
                pop2100 = M2100 + F2100) %>%
         group_by(country_code,name) %>%
         summarize( old2025 = weighted.mean((age %in% old_ages), pop2025),
                    old2100 = weighted.mean((age %in% old_ages), pop2100))

cdist = function(v) {list(x=sort(v), y=seq(v)/length(v))}

plot(cdist(pop$old2025), type='s', lwd=2, 
     main='Distribution of Fraction 75+ Yrs old\nAcross Countries (UN Medium Proj)', xlab='Fraction 75+', ylab='fraction of countries',
     xlim=c(0,.35))

lines(cdist(pop$old2100), type='s', lwd=2, col='red', main='Fraction 75+', xlab='Fraction 75+', ylab='fraction of countries')

plot(density( pop$old2025  , adj=1.5, from=0), lwd=2, ylim=c(0,13), xlim=c(0,.35),
     main='Density of Fraction 75+ Yrs old\nAcross Countries (UN Medium Proj)')
lines(density( pop$old2100  , adj=1.5, from=0), lwd=2, col='red')

