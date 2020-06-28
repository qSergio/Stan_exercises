//
// 
// Vamos a permitir que varíe la ordenada al origen de cada modelo
// y usaremos más niveles

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N; //numero de renglones
  int<lower=0, upper=5> Boro[N];
  vector[N] sqft;
  vector[N] value;
}

// The parameters 
parameters {
  vector[5] a;
  real<lower=0> sigma_y;
  real<lower=0> sigma_a;
  real mu_a;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  mu_a ~ normal(50, 10);
  a ~ normal(mu_a, sigma_a);
  
  for(i in 1:N){
    value[i] ~ normal(a[Boro[i]],sigma_y);
  }
}

