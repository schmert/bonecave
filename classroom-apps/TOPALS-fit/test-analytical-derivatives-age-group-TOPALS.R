#--------------------------------------
# compare the exact Hessian for the 
# TOPALS model with age grouping to the IRLS 
# matrix that Camarda/MortalitySmooth uses as covariance
# 
# the point is to investigate whether
# covariance of TOPALS params is 
# properly estimated by either or
# both formulas
# 
# Conclusion: the "Camarda" IRLS matrix
# is a very good numerical approx to the Hessian, but
# it's not exactly the same. The approximation
# is better near the optimum parameter vector
#--------------------------------------
graphics.off()
rm(list=ls())

library(numDeriv)
library(tidyverse)

# start from a TOPALS fit 

source('compare-Camarda-and-exact-derivs.R')

# copy various constants into memory (B,W, ...)
if (exists('fit')) {
  for (k in names(fit))  assign(k, fit[[k]] )
} else {
  stop('object [fit] does not exist')
}

K = ncol(B)

a = fit$final_alpha   # final fit from TOPALS_fit
z = 0*a               # zero vector
r = runif(K,-1,1)     # random parameter vector

# Poisson Likelihood
Lik = function(a) {
  M = W %*% exp( std + B %*% a)
  Dhat = N*M
  return( sum( dpois( D, Dhat, log=TRUE)))
}

# penalty function
Pen = function(a) {
  as.numeric( -1/2 * t(a) %*% P %*% a)
}

# objective function
Q = function(a) { Lik(a) + Pen(a)}


# approximate Hessian used in MortalitySmooth 
# and in my TOPALS_fit( ) function

Camarda_H = function(a) {
  mu = as.vector( exp( std + B %*% a ))
  M  = as.vector(W %*% mu)
  A  = diag(N/M)
  X  = W %*% diag(mu) %*% B
  return( -(t(X) %*% A %*% X + P))
}

# numerical gradient and Hessian
empirical_g = function(a) { numDeriv::grad(Q, a) }
empirical_H = function(a) { numDeriv::hessian(Q, a) }

# analytical formula for gradient
analytical_g = function(a) {
  mu = as.vector( exp( std + B %*% a ))
  M  = as.vector(W %*% mu)
  
  AA = diag(mu)
  BB = diag(1/M)

  exactg = t(B) %*% AA %*% t(W) %*% BB %*% D -
           t(B) %*% AA %*% t(W) %*% N -
           P %*% a
  

  return(as.vector(exactg))
} # analytical_g


# analytical formula for Hessian

analytical_H = function(a) {

  mu = as.vector( exp( std + B %*% a ))
  M  = as.vector(W %*% mu)
  
  AA = diag(mu)
  BB = diag(1/M)
  
  exactH = matrix(NA,K,K)
  
  for (j in 1:K) {
    bj = as.vector(B[,j])
    
    dAA = diag( bj * mu)
    
    dBB = -diag( as.vector(t(bj) %*% AA %*% t(W) %*% diag((1/M)^2)) ) 

    tmp = t(B) %*% dAA %*% t(W) %*% BB  %*% D +
          t(B) %*%  AA %*% t(W) %*% dBB %*% D -
          t(B) %*% dAA %*% t(W) %*% N -
          P[j,]
    
    exactH[,j] = tmp
  }

    return(exactH)
} # analytical_H



# compare formulas at a and z and random pt

compare = function(this_a) {
  tmp = list(empg = empirical_g(this_a)  %>% round(2), 
             anag = analytical_g(this_a) %>% round(2), 
             empH = empirical_H(this_a)  %>% round(2), 
             CamH = Camarda_H(this_a)    %>% round(2), 
             anaH = analytical_H(this_a) %>% round(2))
  
  print(tmp) 
  
  with(tmp, {
    list(range.diff.Cam = range( CamH - empH),
         range.diff.ana = range( anaH - empH))
  })

}


compare(a)
compare(z)
compare(r)
