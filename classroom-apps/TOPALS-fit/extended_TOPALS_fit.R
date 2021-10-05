#------------------------------------------------------------
# TOPALS fitting function for mortality schedules
# this version keeps the intermediate Newton-Raphson 
# steps
#------------------------------------------------------------

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
      penalty    = 1/2 * t(alpha) %*% P %*% alpha
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
      
      final_i = niter+1  # which row of amat has the final estimates?
        
      return( list( niter             = niter,
                    alpha             = amat[1:final_i,], 
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

target_pop = 500

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

show = function(fit, hue='red') {
  
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
    logmx_true = ITA_HMD_logmx,
    logmx_fit  = fit$logm
  )
  
  this_plot =
    ggplot(data = df_single, aes(x=age,y=logmx_true)) +
    geom_line(aes(x=age,y=std), color='black', lwd=0.5) +
    geom_line(aes(x=age,y=logmx_fit), color=hue, lwd=3, alpha=.40) +
    geom_segment(data=df_grouped,aes(x=L,xend=U,
                                     y=logmx_obs,
                                     yend=logmx_obs),
                 color=hue,lwd=1.5, alpha=.90) +
    geom_point(size=0.60) +
    labs(x='Age',y='Log Mortality Rate',
         title='Italy Females 1980',
         subtitle = paste(sum(fit$D),'deaths to',round(sum(fit$N)),'women')) +
    scale_x_continuous(breaks=c(0,1,seq(5,100,5)),minor_breaks = NULL) +
    scale_y_continuous(limits=c(-10,0),breaks=seq(-10,0,2),minor_breaks = NULL) +
    theme_bw()
  
  print(this_plot)
} # show  

show2 = function(fit, hue='red') {
  
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
    logmx_true = ITA_HMD_logmx,
    logmx_fit  = fit$logm
  )
  
  this_plot =
    ggplot(data = df_single, aes(x=age,y=logmx_true)) +
    geom_line(aes(x=age,y=std), color='black', lwd=0.5) +
    geom_line(aes(x=age,y=logmx_fit), color=hue, lwd=3, alpha=.40) +
    geom_segment(data=df_grouped,aes(x=L,xend=U,
                                     y=logmx_obs,
                                     yend=logmx_obs),
                 color=hue,lwd=1.5, alpha=.90) +
    geom_point(size=0.60) +
    labs(x='Age',y='Log Mortality Rate',
         title='Italy Females 1980',
         subtitle = paste(sum(fit$D),'deaths to',round(sum(fit$N)),'women')) +
    scale_x_continuous(breaks=c(0,1,seq(5,100,5)),minor_breaks = NULL) +
    scale_y_continuous(limits=c(-10,0),breaks=seq(-10,0,2),minor_breaks = NULL) +
    theme_bw()
  
  print(this_plot)
} # show2  


show(fit, 'orangered')

matplot(0:99, fit$std + fit$B %*% t(fit$alpha), type='o')
matplot(seq(nrow(fit$alpha))-1,fit$alpha, type='o',xlab='iteration')

