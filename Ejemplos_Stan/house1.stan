//
// Housing data

data {
  int<lower=0> N;
  vector[N] valor;
  vector[N] sqft;
}

// The parameters accepted by the model. 
parameters {
  real alpha; //intercept
  real beta;
  real<lower=0> sigma;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'alpha + beta*sqft'
// and standard deviation 'sigma'.
model {
  valor ~ normal(alpha + beta*sqft, sigma);
}

