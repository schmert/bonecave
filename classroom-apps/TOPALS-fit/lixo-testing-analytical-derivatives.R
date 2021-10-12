
library(numDeriv)

# start from a TOPALS fit

if (exists('fit')) {
  for (k in names(fit))  assign(k, fit[[k]] )
} else {
  stop('object [fit] does not exist')
}

K = ncol(B)


Lik = function(a) {
  M = W %*% exp( std + B %*% a)
  Dhat = N*M
  return( sum( dpois( D, Dhat, log=TRUE)))
}

Pen = function(a) {
  as.numeric( -1/2 * t(a) %*% P %*% a)
}

Q = function(a) { Lik(a) + Pen(a)}


exactg = function(a) {
  mu = as.vector( exp(std + B %*% a))
  M  = as.vector(W %*% mu)
  part1 = t(B) %*% diag(mu) %*% t(W)
  part2 = as.vector(diag(1/M) %*% D - N)
  return(part1 %*% part2 - P %*% a)
}

delta = .01
g = rep(NA,K)

z = rep(0,K)

for (j in 1:K) {
xp = z + delta/2 * diag(K)[j,]
xm = z - delta/2 * diag(K)[j,]

g[j] = ( Q(xp)-Q(xm) ) / delta
}

g

##################

z = rep(0,7)
a = final_alpha


# test 1: d(mu')/d(theta)

print("d(mu')/d(theta)")

emp = function(a, delta=.001) {
  
  res = matrix(NA, K, length(std))
  
  for (j in 1:K) {
    mp = exp(as.vector( std + B %*% (a + delta/2*diag(K)[j,])))
    mm = exp(as.vector( std + B %*% (a - delta/2*diag(K)[j,])))
    res[j,] = (mp-mm)/delta
  }
  return(res)
}  

ana = function(a) {
  mu  = exp( as.vector( std + B %*% a)  )
  ana = t(B) %*% diag(mu)
}

all(abs( emp(a) - ana(a)) < 1e-4)
all(abs( emp(z) - ana(z)) < 1e-4)

# test 2: d(M')/d(theta)
print("d(M')/d(theta)")

emp = function(a, delta=.001) {
  
  res = matrix(NA, K, nrow(W))
  
  for (j in 1:K) {
    Mp = W %*% exp(as.vector( std + B %*% (a + delta/2*diag(K)[j,])))
    Mm = W %*% exp(as.vector( std + B %*% (a - delta/2*diag(K)[j,])))
    res[j,] = (Mp-Mm)/delta
  }
  return(res)
}  

ana = function(a) {
  mu  = exp( as.vector( std + B %*% a)  )
  ana = t(B) %*% diag(mu) %*% t(W)
}

all(abs( emp(a) - ana(a)) < 1e-4)
all(abs( emp(z) - ana(z)) < 1e-4)

# test 3: d(Dhat')/d(theta)
print("d(Dhat')/d(theta)")

emp = function(a, delta=.001) {
  
  res = matrix(NA, K, nrow(W))
  
  for (j in 1:K) {
    Mp = W %*% exp(as.vector( std + B %*% (a + delta/2*diag(K)[j,])))
    Mm = W %*% exp(as.vector( std + B %*% (a - delta/2*diag(K)[j,])))
    
    Dhatp = N * Mp
    Dhatm = N * Mm
    res[j,] = (Dhatp-Dhatm)/delta
  }
  return(res)
}  

ana = function(a) {
  mu  = exp( as.vector( std + B %*% a)  )
  ana = t(B) %*% diag(mu) %*% t(W) %*% diag(N)
}

all(abs( emp(a) - ana(a)) < 1e-4)
all(abs( emp(z) - ana(z)) < 1e-4)


# test 4: d(logM')/d(theta)
print("d(logM')/d(theta)")

emp = function(a, delta=.001) {
  
  res = matrix(NA, K, nrow(W))
  
  for (j in 1:K) {
    Mp = W %*% exp(as.vector( std + B %*% (a + delta/2*diag(K)[j,])))
    Mm = W %*% exp(as.vector( std + B %*% (a - delta/2*diag(K)[j,])))
    res[j,] = (log(Mp)-log(Mm))/delta
  }
  return(res)
}  

ana = function(a) {
  mu  = exp( as.vector( std + B %*% a)  )
  M   = as.vector(W %*% mu)
  ana = t(B) %*% diag(mu) %*% t(W) %*% diag(1/M)
}

all(abs( emp(a) - ana(a)) < 1e-4)
all(abs( emp(z) - ana(z)) < 1e-4)

# test 5: d(Pen)/d(theta)
# 
print("d(Pen)/d(theta)")

emp = function(a, delta=.001) {
  
  res = rep(NA, K)
  
  for (j in 1:K) {
    h = delta/2*diag(K)[j,]
    Pp =  as.numeric( t(a) %*% P %*% (a+h))
    Pm =  as.numeric(t(a) %*% P %*% (a-h))
    res[j] = (Pp-Pm)/delta
  }
  return(res)
}  


ana = function(a) {
  P %*% a
}


all(abs( emp(a) - ana(a)) < 1e-4)
all(abs( emp(z) - ana(z)) < 1e-4)


# test 6: gradient: dQ/d(theta)
# 
print("dQ/d(theta)")

emp = function(a) {numDeriv::grad(Q,a)}

ana = exactg

all(abs( emp(a) - ana(a)) < 1e-4)
all(abs( emp(z) - ana(z)) < 1e-4)

# test 7: Hessian
print("d2Q/d(theta)d(theta')")

empH = function(a) { numDeriv::hessian(Q,a) }

anaH = function(a) {
  
  mu = as.vector( exp(std + B %*% a))
  M  = as.vector(W %*% mu)
  
  zvec = function(j) {
    bj = as.vector(B[,j])
    
    part1 = diag(bj) %*% diag(mu) %*% t(W) %*% 
      diag(1/M) %*% (D -Dhat)
    
    part2 = diag(mu) %*% t(W) %*% 
      diag( as.vector(W %*% diag(mu) %*% bj )) %*%
      diag( (1/M)^2) %*% D
    
    return(part1 - part2)
  }
  
  Zstar = matrix(NA, length(std), K)
  
  for (j in 1:K) Zstar[,j] = zvec(j)
  
  exactH  = t(B) %*% Zstar - P
  return(exactH)
} #anaH


all(abs( empH(a) - anaH(a)) < 1e-4)
all(abs( empH(z) - anaH(z)) < 1e-4)

###########################
# test 8: d[diag(mu)]/d(theta6)
mu = as.vector( exp( std + B %*% a))

delta = .01
h6    = delta/2 * diag(K)[6,]
mp    = as.vector( exp( std + B %*% (a + h6)))
mm    = as.vector( exp( std + B %*% (a - h6)))
emp    = diag( (mp-mm)/delta)

ana = diag(as.vector(B[,6])) %*% diag(mu)

all( abs( emp-ana < 1e-4))

########################
# test 9: d[diag(1/M)]/d(theta6)
mu = as.vector( exp( std + B %*% a))
M  = as.vector(W %*% mu)

delta = .01
h6    = delta/2 * diag(K)[6,]
Mp    = as.vector( W %*% exp( std + B %*% (a + h6)))
Mm    = as.vector( W %*% exp( std + B %*% (a - h6)))
emp   = diag( (1/Mp-1/Mm)/delta)

b6 = as.vector(B[,6])

ana = -diag(as.vector( W %*% diag(mu) %*% b6)) %*% diag( (1/M)^2)

  
all( abs( emp-ana < 1e-4))

########################
# test 10: d[gi]/d(thetaj)

emp = function(a,i,j) {
  gi = function(a) {numDeriv::grad(Q,a)[i]}  
  numDeriv::grad(gi,a)[j]
} # emp

ana = function(a,i,j) {
  mu = as.vector( exp( std + B %*% a))
  M  = as.vector(W %*% mu)
  bi = as.vector(B[,i])
  bj = as.vector(B[,j])
  
  part1 = t(bi) %*% diag(bj) %*% diag(mu) %*% t(W) %*% diag(1/M) %*% D
  part2 = -t(bi) %*% diag(mu) %*% t(W) %*% diag( as.vector(W %*% diag(mu) %*% bj)) %*%
             diag( (1/M)^2) %*% D
  part3 = -t(bi) %*% diag(bj) %*% diag(mu) %*% t(W) %*% N
  part4 = -P[i,j]
  return( part1 + part2 + part3 + part4)
}

i = 5
j = 4
abs( emp(a,i,j)-ana(a,i,j)) < 1e-3

