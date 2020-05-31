//
// This Stan program defines a simple model:
// Gaussian-Linear model

// The input data is a vector 'y' of length 'N'.
data {
  int I; // esta es la entrada, son los datos;
  int N; // número de muestras;
  real y[N]; //en notación de stan, 
  //[N] a la derecha de la variable es para arreglos 1d
  matrix[I,N] X;
}

// The parameters accepted by the model. 
parameters {
  vector[I] beta;
  real alpha;
  real<lower=0> sigma;
}

// The model to be estimated. 
model { //priors
  beta ~ normal(0,10);
  alpha ~ normal(0,10);
  sigma ~ cauchy(0,10);
  y ~ normal(X * beta + alpha, sigma);
}
