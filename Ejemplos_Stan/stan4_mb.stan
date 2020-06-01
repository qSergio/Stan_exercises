//
// This Stan program defines a not so simple model:
// Hierarchical-Linear model
// Ahora introducimos clases

// The input data is a vector 'y' of length 'N'.
data {
  int I; // esta es la entrada, son los datos;
  int N; // número de muestras;
  int<lower=1> N_subgrupos;
  int<lower=1> indv_to_subgrupo[N];
  int<lower=0, upper=1> y[N];
  matrix[I,N] X;
}

// The parameters accepted by the model. 
parameters {
  vector[I] beta;
  vector[N_subgrupos] alpha; //Cambia a 1 alpha por subgrupo o clase
  real mu_alpha; //media jerárquica
  real<lower=0> sigma_alpha; //desviación jerárquica
}

// The model to be estimated. 
model { //priors
  vector[N] alpha_indv;
  for (n in 1:N)
    alpha_indv[n] = alpha[indv_to_subgrupo[N]];
  
  beta ~ normal(0,10);
  alpha ~ normal(mu_alpha,sigma_alpha);
  
  mu_alpha ~ normal(0,10);
  sigma_alpha ~ cauchy(0,10); 
  //y ~ normal(X * beta + alpha, sigma);
  //y ~ bernoulli(inv_logit(X' * beta + alpha));
  y ~ bernoulli_logit(X' * beta + alpha_indv);
}
