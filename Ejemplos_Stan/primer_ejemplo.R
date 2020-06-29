
# Carga de datos Housing-------------------------------------------------------
library(rstan)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

housing <- read.table('http://jaredlander.com/data/housing1.csv',
                      sep=',',
                      header=T,
                      stringsAsFactors = F)


head(housing)

mod1 <- lm(ValuePerSqFt ~ SqFt, data=housing)
summary(mod1)


# Primeros modelos usando Stan-------------------------------------------------------

house1 <- stan('house1.stan',
               data = list(N=nrow(housing),
                           valor= housing$Value,
                           sqft = housing$SqFt),
               iter=200)

# la salida del modelo y los valores para los parámetros
house1

#visualizar
pairs(house1, pars=c('alpha','beta'))


# PODEMOS USAR MÁS VARIABLES


house2 <- stan('house2.stan',
               data=list(N=nrow(housing),
                         value=housing$ValuePerSqFt,
                         sqft = housing$SqFt,
                         Boro = as.numeric(as.factor(housing$Boro))),
               iter=150
               )

# la salida del modelo y los valores para los parámetros
house2

#visualizar
pairs(house1, pars=c('alpha','beta'))

# Vamos a intentar regresión múltiple -------------------------------------------------------
# Esto es, varias variables

model_mul ="
data {
real y[125];
real x[125,6];
}
parameters {
real beta[6];
real sigma;
real alpha;
}
model {
beta[1] ~ uniform(0,1000);
for (n in 1:125)
y[n] ~ normal(alpha + beta[1]*x[n,1] + beta[2]*x[n,2] + beta[3]*x[n,3] + beta[4]*x[n,4] + beta[5]*x[n,5] + beta[6]*x[n,6], sigma);
}"

xy = list(y=housing[,1],x=housing[,2:7])
fit = stan(model_code = model_mul, 
           data = xy, 
           warmup = 500, 
           iter = 1000, 
           chains = 1, 
           cores = 1, 
           thin = 1,verbose=FALSE)
stan_dens(fit)



