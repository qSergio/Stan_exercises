//
// Primer ejemplo de uso de Stan con R

// Las alturas son un arreglo de N valores
data {
  int<lower=0> N;
  //vector[N] y;
  real Y[N]; 
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real mu;
  real<lower=0> sigma; // la desviación es siempre no negativa
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  for (k in 1:N)
    Y[k] ~ normal(mu, sigma);
  mu ~ normal(1,1);
  sigma ~ cauchy(0,1);
}

