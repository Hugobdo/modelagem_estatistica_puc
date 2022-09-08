############################
###    Autocorrelação    ###
############################


set.seed(8)
dados = rnorm(72)
serie = ts(dados,start = c(2015,1), end=c(2020,12), frequency=12)
print(serie)
plot(serie)
acf(serie)
pacf(serie)

# Teste de Autocorrelação (Ljung-Box)

# Ho = não é autocorrelacionado: p > 0.05
# Ha = é autocorrelacionado: p <= 0.05
Box.test (serie, type = "Ljung")



# Manchas Solares
?sunspots
manchas_solares <- sunspots
manchas_solares
plot(manchas_solares)

# Estacionaridade

# Teste pp (Phillips-Perron)

# Ho = é estacionário: p > 0.05
# Ha = não é estacionário: p <= 0.05
library("urca")
pp <- ur.pp(manchas_solares)
summary(pp)


# Autocorrelação

# Correlograma
acf(manchas_solares)
pacf(manchas_solares)

# Teste de Autocorrelação (Ljung-Box)
# Ho = não é autocorrelacionado: p > 0.05
# Ha = é autocorrelacionado: p <= 0.05
Box.test(manchas_solares, type = "Ljung-Box")







