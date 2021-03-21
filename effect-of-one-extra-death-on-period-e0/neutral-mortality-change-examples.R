## create odd mortality schedules that all have the
## same e0 value

library(tidyverse)
library(HMDHFDplus)

age = 0:110

# --- running the code below, with your HMD username/password
#     will produce the data used next
#
# deaths = readHMDweb(CNTRY='GBRTENW', item='Deaths_1x1',
#                     username=myUN, password = myPW) %>% 
#          filter(Year == 2018) %>% 
#          pull(Total)
# 
# exposure = readHMDweb(CNTRY='GBRTENW', item='Exposures_1x1',
#                     username=myUN, password = myPW) %>% 
#          filter(Year == 2018) %>% 
#          pull(Total)

deaths = c(2571, 172, 74, 75, 58, 68, 58, 52, 49, 43, 
           62, 56, 52, 66, 74, 108, 111, 155, 219, 238, 
           250, 277, 274, 261, 297, 327, 332, 355, 402, 
           407, 451, 478, 472, 540, 593, 624, 643, 782, 
           765, 793, 788, 850, 1009, 1135, 1238, 1418, 
           1599, 1744, 1878, 2066, 2170, 2410, 2606, 
           2850, 2928, 3200, 3385, 3711, 3908, 3976, 4283, 
           4541, 4835, 5196, 5450, 6038, 6395, 6835, 7840, 
           8740, 10085, 11260, 10066, 10918, 11851, 12207, 
           11937, 12411, 13718, 14963, 15909, 16923, 17197,
           18294, 18363, 19034, 20205, 20607, 20434, 19330,18261,
           17131, 15757, 13964, 12183, 10278, 8710, 7235, 5308,
           2692, 1866, 1410, 961, 596, 384, 244.08, 109.98,
           47.56, 20.09, 8.55, 4.75)

exposure = c(669210.73, 691861.02, 712004.53, 715739.78,
             724495.76, 740452.11, 754531.92, 749897.73, 
             734785.26, 724573.81, 726744.81, 708641.67, 
             688934.33, 669226.8, 652195.19, 638155.24, 630686.9,
             646508.56, 665833.27, 690973.51, 712259.35, 
             736907.72, 738219.28, 754322.4, 774900.77, 
             780075.31, 806703.85, 818993, 808465.65, 803667.66, 
             808063.81, 795247.17, 794788.94, 798010.28, 
             780420.81, 782175.75, 782237.78, 786424.28, 
             781489.57, 758138.79, 709801.34, 694317.72, 
             706222.42, 721685.61, 736031.27, 764986.11, 
             791020.76, 819059.8, 801836.14, 820679.92, 
             819766.86, 830890.06, 830177.27, 830330.93, 
             820611.52, 807171.72, 787401.85, 758391.75, 
             724262.96, 713065.81, 690461.42, 665083.58, 
             642302.3, 619905.53, 616813.7, 606548.56, 591775.06,
             594740.49, 603223.79, 619066.1, 644018.51, 
             674298.27, 552941.48, 519673.65, 496971.56, 
             467863.69, 414458.55, 374726.01, 372418.07, 
             364512.81, 349289.51, 323837.66, 299739.11, 
             274434.48, 248359.86, 226145.16, 206983.97, 
             186341.08, 162814.27, 139347.85, 116899.8, 97594.19,
             80572.22, 64176.24, 50281.62, 38456.32, 29221.5, 
             22483.32, 14335.43, 7534.56, 4527.78, 3024.29, 
             1969.86, 1190.85, 687.88, 355.12, 171.99, 78.79, 
             34.08, 14.56, 8.49)


mx = deaths/exposure

e0 = function(D,N) {
  mu = D/N
  H  = cumsum(c(0,mu))
  lx = exp(-H)
  sum( (tail(lx,-1) + head(lx,-1))/2)
}

e0_actual = e0(deaths,exposure)

round(e0_actual, 2)


Tx = function(D,N,x) {
  mu = D/N
  H  = cumsum(c(0,mu))
  lx = exp(-H)
  lx = lx[0:110 >= x]
  sum( (tail(lx,-1) + head(lx,-1))/2)
}

# theoretical effect is -Tx/Nx for one additional
# death at age x

Tvals = sapply(0:110, function(x) Tx(deaths,exposure,x))


## construct a basis for changes in mx that are 
## orthogonal to Tx

e = eigen(tcrossprod(Tvals))
Z = e$vectors[,-1]

i=0
repeat{
 i = i+1  
 delta_mx   = Z %*% rnorm(110, 0, .0001)
 if ((i>100) | all(mx+delta_mx >0)) break
} 

new_mx     = mx + delta_mx
new_deaths = exposure * new_mx

e0(new_deaths, exposure)

plot(age, log(new_mx), type='o')
lines(age, log(mx), col='red',lwd=2)
plot(age,log(new_mx/mx),type='h')

