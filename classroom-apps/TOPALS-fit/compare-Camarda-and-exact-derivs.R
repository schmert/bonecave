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
# converges to the true Hessian, but
# is not identical to the Hessian in
# each step.  In other words, at a point
# where we're partially through the 
# iterations and moving toward a solution,
# the "Camarda matrix" is not the same
# as the approx. curvature of the objective
# function.  But when we're at optimum,
# it *does* equal the Hessian. 
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



dsn = 3
N = data[dsn,] %>% pull(N) %>% unlist()
D = data[dsn,] %>% pull(D) %>% unlist()
L = data[dsn,] %>% pull(L) %>% unlist()
H = data[dsn,] %>% pull(H) %>% unlist()

boundaries = c(L, tail(H,1))
std = male_std


fit = TOPALS_fit( N, D, std, age_group_bounds = boundaries, 
                  pause_at_iter = 7,
                  details=TRUE)

z = function(j) {
  bj = as.vector(fit$B[,j])
  part1 = diag(bj) %*% diag(fit$mu) %*% t(fit$W) %*% 
              diag(1/fit$M) %*% (fit$D -fit$Dhat)
  
  part2 = diag(fit$mu) %*% t(fit$W) %*% 
           diag( as.vector(fit$W %*% diag(fit$mu) %*% bj )) %*%
           diag( (1/fit$M)^2) %*% fit$D
  
  return(part1 - part2)
}

Zstar = matrix(NA, 100, 7)

for (j in 1:7) Zstar[,j] = z(j)


exactH  = -solve( t(fit$B) %*% Zstar - fit$P)
approxH = fit$covar 

## simulate the Hessian


## log lik function (without scaling constant)
Lik = function(alpha) {
  M = fit$W %*% exp( fit$std + fit$B %*% alpha)
  likelihood = sum(fit$D * log(fit$M) - fit$N * fit$M)
  return( likelihood )
}

## penalty function
Pen = function(alpha) {
  penalty    = -1/2 * t(alpha) %*% fit$P %*% alpha
  return( penalty )
}

Q = function(a) {Lik(a) + Pen(a)}

delta = .001

gradient = function(a) {
  K = ncol(fit$B)
  dv = rep(NA,K)
  
  for (i in 1:K) {
    tmp1 = Q( a + delta/2*diag(K)[i,])
    tmp2 = Q( a - delta/2*diag(K)[i,])
    dv[i] = (tmp2-tmp1)/delta    
  }
  return(dv)
}

gradient(fit$final_alpha)

K = ncol(fit$B)
HH = matrix(NA,K,K)

for (i in 1:K) {
  tmp1 = gradient( fit$final_alpha + delta/2*diag(K)[i,] )
  tmp2 = gradient(fit$final_alpha  - delta/2*diag(K)[i,] )
  HH[i,] = (tmp2-tmp1)/delta
}

gradient(fit$final_alpha)

HH

## numerical essian



(1e4 * exactH)  %>%  round()
(1e4 * approxH) %>%  round()

(1e4 * (approxH - exactH)) %>% round()

