library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)


# Y van a ser alturas de individuos
# se distribuyen normal y queremos la media mu y desviación sigma
# vamos a dar aprioris muy informativas ya que la media también
# asumiresmoq ue se distribuye normal con media 1.7 
# y desviación de 0.3
# 
# Generamos los datos de alturas a modo de ejemplo:
N = 100
Y = rnorm(N,1.6,0.2)
hist(Y)

# compilamos el modelo de stan

modelo = stan_model("alturas.stan")

# falta pasarle los datos

fit = sampling(modelo, list(N=N, Y=Y), iter=200, chains=4)

# diagnóstico de la salida
print(fit)

#graficamos

params= extract(fit)

hist(params$mu)
hist(params$sigma)

library(shinystan)
launch_shinystan(fit)
