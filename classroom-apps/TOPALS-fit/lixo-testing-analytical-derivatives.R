# start from a TOPALS fit

if (exists('fit')) {
  for (k in names(fit))  assign(k, fit[[k]] )
}

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
  M  = W %*% mu
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



