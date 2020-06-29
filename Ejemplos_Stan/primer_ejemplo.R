
# Carga de datos Housing-------------------------------------------------------
library(rstan)
library(dplyr)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

housing <- read.table('http://jaredlander.com/data/housing1.csv',
                      sep=',',
                      header=T,
                      stringsAsFactors = F)
saveRDS(housing,"housing.rds")

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
real y[481];
real x[481,6];
}
parameters {
real beta[6];
real sigma;
real alpha;
}
model {
beta[1] ~ uniform(0,1000);
for (n in 1:481)
y[n] ~ normal(alpha + beta[1]*x[n,1] + beta[2]*x[n,2] + beta[3]*x[n,3] + beta[4]*x[n,4] + beta[5]*x[n,5] + beta[6]*x[n,6], sigma);
}"
read.table('http://jaredlander.com/data/housing1.csv',
           sep=',',
           header=T,
           stringsAsFactors = F)

house_prices <- read.table("https://raw.githubusercontent.com/fjuretig/R_statistics_cookbook/master/chapter03/house_prices.csv",
                           sep=',',
                           header=T,
                           stringsAsFactors = F)
saveRDS(house_prices,"housing_prices.rds")

#housin_nna <- housing %>% 
#  filter(!is.na(YearBuilt))

#house_nna <- housing[complete.cases(housing), ]

xy = list(y=house_prices[,1],x=house_prices[,2:7])
fit = stan(model_code = model_mul, 
           data = xy, 
           warmup = 500, 
           iter = 1000, 
           chains = 3, 
           cores = 3, 
           thin = 1,verbose=FALSE)
stan_dens(fit)

traceplot(fit)
summary(fit)


# Vamos a intentar regresión múltiple -------------------------------------------------------
# imponemos priors más estrictas o "informativas"...

model_mu2 ="
data {
real y[481];
real x[481,6];
}
parameters {
real beta[6];
real sigma;
real alpha;
}
model {
beta[1] ~ uniform(0,1000);
 
beta[2]  ~ normal(1,0.5);  
beta[3]  ~ normal(1,0.1);  
beta[4]  ~ normal(1,0.1);  
beta[5]  ~ normal(1,0.1);  
beta[6]  ~ normal(1,0.1);

for (n in 1:481)
  y[n] ~ normal(alpha + beta[1]*x[n,1] + beta[2]*x[n,2] + beta[3]*x[n,3] + beta[4]*x[n,4] + beta[5]*x[n,5] + beta[6]*x[n,6], sigma);
}"

fit = stan(model_code = model_mu2, 
           data = xy, 
           warmup = 500, 
           iter = 2000, 
           chains = 3, 
           cores = 3, 
           thin = 1,verbose=FALSE)
stan_dens(fit)

traceplot(fit)
summary(fit)

