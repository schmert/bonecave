#------------------------------------------------------------
# TOPALS fitting function for mortality schedules
# this version keeps the intermediate Newton-Raphson 
# steps
#------------------------------------------------------------

library(tidyverse)

graphics.off()

TOPALS_fit = function( N, D, std,
                       age_group_bounds   = 0:100,
                       knot_positions     = c(0,1,10,20,40,70), 
                       penalty_precision  = 2,
                       max_iter           = 50,
                       alpha_tol          = .00005,
                       details            = FALSE) {

    require(splines)
  
    ## single years of age from 0 to (A-1)
    A   = length(std)
    age = 0:(A-1)

    ## B is an AxK matrix. Each column is a linear B-spline basis function
    B      = bs( age, knots=knot_positions, degree=1 )
    K = ncol(B) 
    
    D1 = diff( diag(K), diff=1)
    P  = penalty_precision * crossprod(D1)
    
    ## number and width of age groups
    G     = length(age_group_bounds)-1   
    nages = diff(age_group_bounds)
    
    ## weighting matrix for mortality rates (assumes uniform
    ## distribution of single-year ages within groups)
    W = matrix(0, nrow=G, ncol=A, 
               dimnames=list(head(age_group_bounds,-1) , age))

    offset = 0
    for (g in 1:G) {
      W[g, offset + 1:nages[g]] = 1/nages[g]
      offset = offset + nages[g]
    }

    ## log lik function (without scaling constant)
    Lik = function(alpha) {
      M = W %*% exp( std + B %*% alpha)
      likelihood = sum(D * log(M) - N * M)
      return( likelihood )
    }
    
    ## penalty function
    Pen = function(alpha) {
      penalty    = -1/2 * t(alpha) %*% P %*% alpha
      return( penalty )
    }
    
    
    #------------------------------------------------
    # iteration function: 
    # next alpha vector as a function of current alpha
    #------------------------------------------------
    next_alpha = function(alpha) {
      mu = as.vector( exp( std + B %*% alpha))
      M  = as.vector( W %*% mu)
      
      Dhat = N * M
      
      X = W %*% diag(mu) %*% B
      A = diag(N/M)
      
      y = (D-Dhat)/N + X %*% alpha
      
      updated_alpha = solve( t(X) %*% A %*% X + P, t(X) %*% A %*% y)
      return(as.vector(updated_alpha))
    }
    
    ## main iteration:     
    amat     = matrix(NA, nrow=max_iter, ncol=K)
    amat[1,] = rep(0, K)
    
    niter = 0
    repeat {
      niter          = niter + 1  # eg, 1st iteration
      
      this_i         = niter + 1  # eg, 2nd row of amat produced by 1st iter
      last_i         = niter      
      amat[this_i,]  = next_alpha( amat[last_i,] )  # update
      change         = amat[this_i,] - amat[last_i,]

      converge = all( abs(change) < alpha_tol )
      overrun  = (niter == max_iter)
      
      if (converge | overrun) { break }
      
    } # repeat
    
    if (details | !converge | overrun) {
      if (!converge) print('did not converge')
      if (overrun) print('exceeded maximum number of iterations')
      
      mu    = as.vector( exp(std + B %*% amat[niter,]))
      M     = as.vector( W %*% mu )
      dhat  = N * M
      
      X     = W %*% diag(mu) %*% B
      A     = diag(N/M)
      
      covar = solve( t(X) %*% A %*% X + P)
      
      final_i     = niter+1  # which row of amat has the final estimates?
      final_alpha = amat[final_i,]
      
      return( list( niter             = niter,
                    alpha             = amat[1:final_i,],
                    final_alpha       = final_alpha,
                    D                 = D,
                    N                 = N,
                    age_group_bounds  = age_group_bounds,
                    knots             = knot_positions,
                    std               = std,
                    B                 = B,
                    logm              = std + B %*% amat[final_i,],
                    M                 = W %*% mu,
                    Dhat              = N * M,
                    covar             = covar,
                    Lik               = apply(amat[1:final_i,], 1, Lik),
                    Pen               = apply(amat[1:final_i,], 1, Pen),
                    converge          = converge, 
                    maxiter           = overrun))
    } else return( amat[niter,]) 
    
} # TOPALS_fit

### Italian female 1980 HMD data for age groups
boundaries = c(0,1,seq(5,85,5)) # last group is [80,85)

N = c(312106.85, 1423566.3, 2105814.63, 2249555.41, 2230885.74, 1983157.8,
      1874479.58, 1989351.99, 1772133.73, 1896866.51, 1836597.36, 1834496.64,
      1811178.38, 1192763.85, 1498384.03, 1223810.9, 863725.92, 537720.77)

D = c(3889, 716, 587, 589, 791, 816, 832, 1257, 1651, 2721, 4310,
      6636, 10536, 11043, 23312, 34945, 44537, 50392)

names(N) = names(D) = head(boundaries,-1)

## standard schedule is (very arbitrarily!) the 1959 period
## schedule for Canadian females at ages 0,1,...,99
## these are the log mortality rates

std = c(-3.8933, -5.7776, -6.8474, -7.3298, -7.4519, -7.4408, -7.4807,
        -7.5845, -7.7219, -7.8628, -7.9771, -8.041, -8.0568, -8.0329,
        -7.9779, -7.9005, -7.8088, -7.7101, -7.6113, -7.5195, -7.4415,
        -7.3823, -7.3393, -7.308, -7.2837, -7.2619, -7.238, -7.2082,
        -7.1712, -7.1264, -7.0735, -7.0118, -6.9414, -6.8648, -6.7849,
        -6.7047, -6.6272, -6.5544, -6.4845, -6.4147, -6.3423, -6.2645,
        -6.1791, -6.0872, -5.9904, -5.8903, -5.7887, -5.6869, -5.586,
        -5.4866, -5.3895, -5.2953, -5.2049, -5.1186, -5.0347, -4.9513,
        -4.8664, -4.778, -4.6847, -4.5877, -4.4887, -4.3895, -4.2918,
        -4.1969, -4.1041, -4.0122, -3.9199, -3.8261, -3.7296, -3.6303,
        -3.5278, -3.4221, -3.3129, -3.2004, -3.0861, -2.9716, -2.8589,
        -2.7497, -2.6458, -2.5482, -2.4556, -2.3659, -2.2771, -2.187,
        -2.0942, -1.9991, -1.9028, -1.8062, -1.7105, -1.6164, -1.5242,
        -1.434, -1.3458, -1.2596, -1.1758, -1.0958, -1.0212, -0.9535,
        -0.8944, -0.8454)

big_fit = TOPALS_fit(N,D,std,
                 age_group_bounds = boundaries,
                 details=TRUE)
str(big_fit)



target_pop = 5000

smallN = N * target_pop/sum(N)
smallD = rpois(length(smallN), smallN* D/N)

fit = TOPALS_fit(smallN, smallD,std,
                 age_group_bounds = boundaries,
                 detail=TRUE)

plot(fit$D, fit$Dhat)
abline(0,1)
text(fit$D, fit$Dhat, label=head(boundaries,-1), col=2)


z = tibble(x=head(boundaries,-1), smallN, smallD, Dhat=fit$Dhat)

ggplot(data=z) + 
  geom_point(aes(x=x,y=smallD),size=3) + 
  geom_point(aes(x=x,y=Dhat), shape=16,color='red',size=3, alpha=.60)

## single-year log mortality rates from HMD
## these are the targets for TOPALS estimation
ITA_HMD_logmx = 
  c(-4.3852, -7.1185, -7.6009, -7.7517, -8.1117, -8.1456, -8.1456, 
    -8.1456, -8.294, -8.2171, -8.4684, -8.294, -8.3349, -8.1456, 
    -8.0789, -7.9866, -7.9866, -8.0164, -7.902, -7.824, -7.7753, 
    -7.7753, -7.7753, -7.7753, -7.8753, -7.7063, -7.7063, -7.6628, 
    -7.6417, -7.8753, -7.4876, -7.4354, -7.2644, -7.3233, -7.3385, 
    -7.2644, -7.0021, -6.959, -6.959, -6.7855, -6.8216, -6.5713, 
    -6.5225, -6.4956, -6.3539, -6.2712, -6.2196, -6.0035, -5.9835, 
    -5.8569, -5.7992, -5.7169, -5.6694, -5.5315, -5.433, -5.3247, 
    -5.2514, -5.1814, -5.0625, -4.9533, -4.8783, -4.7915, -4.6767, 
    -4.5923, -4.4945, -4.3836, -4.2992, -4.1825, -4.0513, -3.9409, 
    -3.8135, -3.6913, -3.5332, -3.4455, -3.2966, -3.2069, -3.0614, 
    -2.9677, -2.8466, -2.7201, -2.5974, -2.4617, -2.3462, -2.2249, 
    -2.1253, -1.9713, -1.8905, -1.7861, -1.6842, -1.5945, -1.4583, 
    -1.3792, -1.297, -1.2087, -1.1393, -1.0245, -0.9444, -0.8681, 
    -0.7958, -0.7276)

show = function(fit, hue='red', ti='') {
  
  df_grouped = data.frame(
    L = head( fit$age_group_bounds, -1),
    U = tail( fit$age_group_bounds, -1),
    N = fit$N,
    D = fit$D
  ) %>%
    mutate(logmx_obs = log(D/N))
  
  df_single  = data.frame(
    age=  seq(fit$std) - .50,  # 0.5, 1.5, ...
    std = fit$std,
    logmx_fit  = fit$logm
    )
  
  # simulate 10th and 90th pointwise intervals at each age
  CH  = t( chol( fit$covar ))
  m   = fit$final_alpha
  sim = replicate(10000, {a=m + CH %*% rnorm(7); as.vector(fit$std + fit$B %*% a)})
  
  df_single$Q10 = apply(sim,1,quantile,prob=.10)
  df_single$Q90 = apply(sim,1,quantile,prob=.90)
  
  mx_vals =  c(1,2,10,20,100,200,1000,2000,10000)
  
  this_plot =
    ggplot(data = df_single, aes(x=age,y=std)) +
    geom_line(aes(x=age,y=std), color='black', lwd=0.5) +
    geom_line(aes(x=age,y=logmx_fit), color=hue, lwd=3, alpha=.40) +
    geom_segment(data=df_grouped,aes(x=L,xend=U,
                                     y=logmx_obs,
                                     yend=logmx_obs),
                 color=hue,lwd=1.5, alpha=.90) +
    geom_point(size=0.60) +
    geom_ribbon(aes(x=age,ymin=Q10,ymax=Q90), fill=hue, alpha=.10) +
    labs(x='Age',y='Mortality Rate per 10,000 (Log Scale)',
         title=ti, subtitle = paste(round(sum(fit$D)),'deaths in',round(sum(fit$N)),'person-years')) +
    scale_x_continuous(breaks=c(0,1,seq(5,100,5)),minor_breaks = NULL) +
    scale_y_continuous(limits=range(c(-10,0,df_single$Q10,df_single$Q90)),
                       breaks=log(mx_vals /10000),
                       minor_breaks = NULL,
                       labels=paste(mx_vals)) +
    theme_bw()
  
  print(this_plot)
} # show  



matplot(0:99, fit$std + fit$B %*% t(fit$alpha), type='o')

ii = seq(nrow(fit$alpha))-1
matplot(ii,fit$alpha, type='o',xlab='iteration')

plot(ii,fit$Pen/fit$Lik, type='o', main='Pen/Lik')

show(fit, 'orangered', 'Simulated Population')


############ delete from here down after testing

SaoBorja = structure(list(L = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 
                                13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 
                                29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 
                                45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 
                                61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 
                                77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 
                                93, 94, 95, 96, 97, 98, 99), 
                          H = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 
                              10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 
                              26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 
                              42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 
                              58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 
                              74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 
                              90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100), 
                          Deaths = c(1, 0, 
                                     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 
                                     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                     0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 
                                     0, 0, 1, 1, 0, 0, 3, 0, 2, 0, 1, 0, 1, 0, 1, 0, 1, 3, 2, 0, 2, 
                                     2, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1), 
                          Exposure = c(63.7, 
                                  57.8, 56.8, 52.8, 54.7, 60, 69.1, 79.5, 75.4, 73, 71.2, 83.4, 
                                  90.4, 85.6, 79.6, 86.4, 88.3, 88.4, 84.4, 90.7, 91, 98.4, 101, 
                                  102.3, 89.6, 88.2, 86.6, 89.3, 80.4, 76.2, 71.7, 72.5, 74.4, 
                                  74.2, 78.7, 81.5, 88.1, 88.6, 87.9, 75.4, 70.8, 74.5, 87.1, 91.5, 
                                  81.7, 73.6, 79.6, 83.8, 90.7, 86.2, 86.1, 80.1, 80.8, 81.9, 82.5, 
                                  72.9, 80.1, 75.6, 78.5, 67.7, 70, 67.8, 64.2, 54.2, 45.4, 40, 
                                  41.8, 46.2, 48.4, 46.3, 40.9, 37.3, 35.8, 34.1, 34.1, 36.9, 35.4, 
                                  31.8, 30.1, 30.5, 27.9, 26, 24.7, 23.5, 15.9, 9.1, 4.7, 5.7, 
                                  7.2, 6.5, 4.6, 3, 2, 1.9, 1.1, 0.6, 0.4, 1.3, 1, 0.6)), 
                     class = c("tbl_df",                                                                                                                        
                               "tbl", "data.frame"), row.names = c(NA, -100L))
 
tmp = SaoBorja
fit = TOPALS_fit(N=tmp$Exposure, D= tmp$Deaths, std=std, 
                 age_group_bounds = c(tmp$L,100), details=TRUE)
show(fit, hue='blue', ti="São Borja RS 2009-2011")

##
LittleRock = structure(list(L = c(0L, 5L, 18L, 21L, 25L, 45L, 55L, 60L, 65L, 
                                  75L, 85L), 
                            H = c(4L, 17L, 20L, 24L, 44L, 54L, 59L, 64L, 74L, 
                                                   84L, 99L), 
                            Deaths = c(91L, 23L, 13L, 16L, 146L, 104L, 72L, 109L, 
                                                                         392L, 589L, 508L), 
                            Exposure = c(12741, 31012, 7374, 11076, 61002, 
                                           16878, 6753, 6888, 12190, 7384, 2497)), 
                       class = c("tbl_df", "tbl",  "data.frame"), row.names = c(NA, -11L))

tmp = LittleRock
fit = TOPALS_fit(N=tmp$Exposure, D= tmp$Deaths, 
                 age_group_bounds = c(tmp$L,100),
                 std=std, details=TRUE)
show(fit, hue='orange', ti="Little Rock 1990")

ISL = structure(list(L = c(0L, 1L, 5L, 10L, 15L, 20L, 25L, 30L, 35L, 
                           40L, 45L, 50L, 55L, 60L, 65L, 70L, 75L, 80L, 85L, 90L, 95L), 
                     H = c(1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 
                           70, 75, 80, 85, 90, 95, 100), 
                     Deaths = c(2, 3, 5, 2, 0, 5, 
                          5, 5, 10, 13, 13, 23, 24, 37, 41, 77, 97, 178, 173, 117, 
                          46), 
             Exposure = c(2014.96, 8382.31, 10890.62, 11096.16, 10144.15, 
                        10935.01, 10522.14, 9902.84, 10864.81, 10556.26, 9636.91, 
                        8412.53, 6773.92, 5073.68, 4869.78, 4723.54, 3838.91, 2694.91, 
                        1517.95, 623.48, 164.55)), 
                row.names = c(NA, -21L), class = "data.frame")  
  tmp = ISL
  fit = TOPALS_fit(N=tmp$Exposure, D= tmp$Deaths, std=std, 
                   age_group_bounds=c(tmp$L,100),details=TRUE)
  show(fit, hue='purple', ti="Iceland Females 2002")
  
RUS =  structure(list(L = c(0L, 1L, 5L, 10L, 15L, 20L, 25L, 30L, 35L, 
                            40L, 45L, 50L, 55L, 60L, 65L, 70L, 75L, 80L, 85L, 90L, 95L), 
                      H = c(1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 
                            70, 75, 80, 85, 90, 95, 105), 
                      Deaths = c(142382.89, 35103.61, 
                               20844.88, 15661.23, 45332.9, 101355.55, 132479.93, 148928.9, 
                               118267.4, 236438.23, 285552.75, 406375.72, 378929.32, 279944.86, 
                               371661.07, 430455.41, 336869.39, 202931.61, 106637.51, 38754.7, 
                               10284.95), 
                      Exposure = c(5854168.07, 22051398.7, 26280605.34, 
                                 24147608.61, 26175480.59, 32744750.36, 31878566.42, 27921974.27, 
                                16963977.69, 23888101.88, 21827019.46, 22012556.51, 15335986.02, 
                                  7990749.5, 7475667.82, 6063199.91, 3243859.02, 1300877.55, 
                                 481510.01, 134707.34, 31306.19)), 
                 row.names = c(NA, -21L), class = "data.frame") 


tmp = RUS
fit = TOPALS_fit(N=tmp$Exposure, D= tmp$Deaths, std=std, 
                 age_group_bounds=c(tmp$L,100),details=TRUE)
show(fit, hue='darkgreen', ti="Russia 1980-1984")

# silly experiment: refit using Arkansas male 1990 as standard
mx = 
  c(0.02738, 0.00221, 0.00129, 8e-04, 0.00059, 0.00054, 0.00048, 
    0.00054, 0.00036, 0.00041, 6e-04, 0.00052, 0.00042, 0.00067, 
    0.00088, 0.0011, 0.00115, 0.00146, 0.00149, 0.00152, 0.0016, 
    0.00189, 0.00185, 0.0014, 0.00191, 0.00141, 0.0013, 0.00165, 
    0.00127, 0.00208, 0.0016, 0.00203, 0.0017, 0.00214, 0.00224, 
    0.00231, 0.00267, 0.00234, 0.00346, 0.00316, 0.00295, 0.00293, 
    0.00441, 0.00418, 0.00385, 0.00513, 0.00482, 0.00583, 0.00619, 
    0.00649, 0.00759, 0.00748, 0.0082, 0.00857, 0.01068, 0.00982, 
    0.01031, 0.01091, 0.01479, 0.01511, 0.01504, 0.01751, 0.01961, 
    0.01948, 0.01985, 0.02369, 0.02522, 0.02949, 0.02944, 0.03234, 
    0.0365, 0.03388, 0.0419, 0.0476, 0.04942, 0.05781, 0.04908, 0.06636, 
    0.06914, 0.08174, 0.09346, 0.089, 0.10525, 0.1113, 0.13789, 0.13177, 
    0.16051, 0.16474, 0.16704, 0.17884, 0.19128, 0.20437, 0.21811, 
    0.23249, 0.24751, 0.26314, 0.27937, 0.29618, 0.31351, 0.33135
  )

sp = smooth.spline(x=0:99, y=log(mx) )
sm = predict(sp,0:99)$y

tmp = RUS
fit2 = TOPALS_fit(N=tmp$Exposure, D= tmp$Deaths, std= sm, 
                 age_group_bounds=c(tmp$L,100),details=TRUE)

show(fit2, hue='darkgreen', ti="Russia 1980-1984, alt standard")


