functions {
  // random walk 1 density
   real rw1_lpdf(vector y , real sigma) {
     int n = num_elements(y);
     vector[n-1] ydiff;
     
     ydiff = y[2:n] - y[1:(n-1)];
     
     return( normal_lpdf(ydiff | 0, sigma ));
   } // rw1_lpdf
}
data {
  int<lower=1> nyrs;
  int<lower=1989,upper=2018> year[nyrs];
  int<lower=0> z[nyrs];
  int<lower=0> n[nyrs];
}

// 
parameters {
  vector<lower=0,upper=1>[30] theta;  // latent true probs for 1989,1990,...,2018
  real<lower=0> sigma;
}

model {
  int ix;

// binomial likelihood
  for (i in 1:nyrs) {
    ix = year[i] - 1988;
    z[i] ~ binomial( n[i], theta[ix]) ;
  }
  
// priors
  theta ~ rw1(sigma);
  sigma ~ normal(0,.10);
}

