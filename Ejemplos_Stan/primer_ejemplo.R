
# Carga de datos ----------------------------------------------------------


housing <- read.table('http://jaredlander.com/data/housing1.csv',
                      sep=',',
                      header=T,
                      stringsAsFactors = F)


head(housing)

mod1 <- lm(ValuePerSqFt ~ SqFt, data=housing)
summary(mod1)

library(rstan)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

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

