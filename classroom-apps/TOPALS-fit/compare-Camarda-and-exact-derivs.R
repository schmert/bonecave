#--------------------------------------
# compare the exact Hessian for the 
# TOPALS model to the IRLS matrix
# that Camarda uses as covariance
# 
# the point is to investigate whether
# covariance of TOPALS params is 
# properly estimated by either or
# both formulas
# 
# Conclusion: the "Camarda" matrix
# is a good numerical approx to the true Hessian
#--------------------------------------

library(tidyverse)
library(splines)

# load example data ----
# retrieve example data and HMD-based standards
load('input_datasets.Rdata')


TOPALS_fit = function( N, D, std,
                       age_group_bounds   = 0:100,
                       knot_positions     = c(0,1,10,20,40,70), 
                       penalty_precision  = 2,
                       max_iter           = 50,
                       pause_at_iter      = 50,
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
    pause    = (niter == pause_at_iter)
    
    if (converge | overrun | pause) { break }
    
  } # repeat
  
  if (details | !converge | overrun | pause) {
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
    
    logm              = as.vector( std + B %*% final_alpha )
    
    
    return( list( niter             = niter,
                  alpha             = amat[1:final_i,],
                  final_alpha       = final_alpha,
                  D                 = D,
                  N                 = N,
                  age_group_bounds  = age_group_bounds,
                  knots             = knot_positions,
                  std               = std,
                  B                 = B,
                  W                 = W,
                  P                 = P,
                  logm              = logm,
                  mu                = exp(logm),
                  M                 = as.vector(W %*% mu),
                  Dhat              = N * M,
                  covar             = covar,
                  Lik               = apply(amat[1:final_i,], 1, Lik),
                  Pen               = apply(amat[1:final_i,], 1, Pen),
                  converge          = converge, 
                  maxiter           = overrun))
  } else return( amat[niter,]) 
  
} # TOPALS_fit



dsn = 2
N = data[dsn,] %>% pull(N) %>% unlist()
D = data[dsn,] %>% pull(D) %>% unlist()
L = data[dsn,] %>% pull(L) %>% unlist()
H = data[dsn,] %>% pull(H) %>% unlist()

boundaries = c(L, tail(H,1))
std = male_std


fit = TOPALS_fit( N, D, std, age_group_bounds = boundaries, 
                  pause_at_iter = 99, alpha_tol = 5e-7,
                  details=TRUE)
show(fit)