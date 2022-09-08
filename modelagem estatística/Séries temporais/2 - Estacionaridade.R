######################################
###    TESTE DE ESTACIONARIDADE    ###
######################################


set.seed(10)
dados1 <- rnorm(41)
serie1 <- ts(dados1, start = 1980, end=2020, frequency=1)
plot(serie1)


# ---------------------------------------------------
# Teste KPSS (Kwiatkowski-Phillips-Schmidt-Shin)

# Ho = não é estacionário: teste estatístico > valor crítico
# Ha = é estacionário:  teste estatístico < valor crítico
library ("urca")
kpss <- ur.kpss(serie1)
summary(kpss)


# Teste pp (Philips-Perron)

# Ho = é estacionário: p > 0.05
# Ha = não é estacionário: p <= 0.05
pp <- ur.pp(serie1)
summary(pp)

# Teste df (Dickey Fuller)

# Ho = não é estacionário: teste estatístico > valor crítico
# Ha = é estacionário:  teste estatístico < valor crítico
df <- ur.df(serie1)
summary(df)


# ---------------------------------------------------
library(help = "datasets")

# Número de passageiros aéreos entre 1949 a 1960
?AirPassengers 

AirPassengers
plot(AirPassengers)


# Teste KPSS (Kwiatkowski-Phillips-Schmidt-Shin)

# Ho = não é estacionário: teste estatístico > valor crítico
# Ha = é estacionário:  teste estatístico < valor crítico
kpss2 <- ur.kpss(AirPassengers) 
summary(kpss2)

# Teste pp (Philips-Perron)

# Ho = é estacionário: p > 0.05
# Ha = não é estacionário: p <= 0.05
pp2 <- ur.pp(AirPassengers)
summary(pp2)


# Teste df (Dickey Fuller)

# Ho = não é estacionário: teste estatístico > valor crítico
# Ha = é estacionário:  teste estatístico < valor crítico
df2 <- ur.df(AirPassengers)
summary(df2)


