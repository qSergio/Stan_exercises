//
// This Stan program defines a not so simple model:
// Hierarchical-Linear model
// Aun no hay clases internas, solo parámetros jerárquicos

// The input data is a vector 'y' of length 'N'.
data {
  int I; // esta es la entrada, son los datos;
  int N; // número de muestras;
  int<lower=0, upper=1> y[N];
  matrix[I,N] X;
}

// The parameters accepted by the model. 
parameters {
  vector[I] beta;
  vector[N] alpha; // N alphas, una para cada individuo
  real mu_alpha; //media jerárquica
  real<lower=0> sigma_alpha; //desviación jerárquica
}

// The model to be estimated. 
model { //priors
  beta ~ normal(0,10);
  alpha ~ normal(mu_alpha,sigma_alpha);
  mu_alpha ~ normal(0,10);
  sigma_alpha ~ cauchy(0,10); 
  //y ~ normal(X * beta + alpha, sigma);
  //y ~ bernoulli(inv_logit(X' * beta + alpha));
  y ~ bernoulli_logit(X' * beta + alpha);
}
