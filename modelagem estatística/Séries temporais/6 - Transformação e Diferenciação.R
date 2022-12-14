############################################
###    Transforma??o e Diferencia??o     ###
############################################

#Transforma??o: busca-se melhorar o grau de normalidade
#Diferencia??o:transformar dados n?o estacion?rios em estacion?rios

# ---------------------------------------------------------------
#EXEMPLO 1

#Transforma??o Logaritmica
# S?rie mensal
set.seed(6)
dados <- rnorm(72, 10, 1)
serie1 <- ts(dados,start = c(2015,1), end=c(2020,12), frequency=12)
print(serie1)
plot(serie1)
summary(serie1)

# NORMALIDADE
# Gr?fico
qqnorm(serie1)
qqline(serie1)

# Ho = distribui??o normal : p > 0.05
# Ha = distribui??o != normal : p <= 0.05
shapiro.test(serie1)
hist(serie1)


# Transforma??o por log (Diminuir vari?ncia e melhorar normalidade)
serie2 <- log(serie1)

# NORMALIDADE
hist(serie2)

# Gr?fico
qqnorm(serie2)
qqline(serie2)

# Ho = distribui??o normal : p > 0.05
# Ha = distribui??o != normal : p <= 0.05
shapiro.test(serie2)


# -------------------------
#Transforma??o Exponencial
# Transforma??o por raiz c?bica (quando possui dados com valor zero ou negativos)
serie3 <- (serie1)^(1/3)

# NORMALIDADE
hist(serie3)

# Gr?fico
qqnorm(serie3)
qqline(serie3)

# Ho = distribui??o normal : p > 0.05
# Ha = distribui??o != normal : p <= 0.05
shapiro.test(serie3)


# ------------------------
## Diferencia??o
# Estacionaridade

library(forecast)
decomposicao <- decompose(serie2)
autoplot(decomposicao)

# Teste pp (Philips-Perron)

# Ho = ? estacion?ria: p > 0.05
# Ha = n?o ? estacion?ria: p <= 0.05
library("urca")
pp <- ur.pp(serie2)
summary(pp)

# Indica??o de quantas diferencia??es devem ser feitas
ndiffs(serie2)



# ---------------------------------------------------------------
#EXEMPLO 2
# N?mero de passageiros a?reos entre 1949 a 1960
passageiros <- AirPassengers
plot(passageiros)


# NORMALIDADE

# Gr?fico
qqnorm(passageiros)
qqline(passageiros)

# Ho = distribui??o normal : p > 0.05
# Ha = distribui??o != normal : p <= 0.05
shapiro.test(passageiros)
hist(passageiros)

#------------------------
# Transforma??o (Diminuir vari?ncia e melhorar normalidade)

#Logaritmica
passageiros2 <- log(passageiros)
hist(passageiros2)
shapiro.test(passageiros2)

#Raiz C?bica
passageiros3 <- passageiros^(1/3)
hist(passageiros3)
shapiro.test(passageiros3)


qqnorm(passageiros2)
qqline(passageiros2)

qqnorm(passageiros3)
qqline(passageiros3)


#------------------------
## Diferencia??o
# Estacionaridade

library(forecast)
decomposicao <- decompose(passageiros3)
autoplot(decomposicao)

# Indica??o de quantas diferencia??es devem ser feitas
ndiffs(passageiros3)
#Se uma n?o deixar a s?rie estacion?ria, n?o adianta fazer outra

# Teste pp (Philips-Perron)

# Ho = ? estacion?rio: p > 0.05
# Ha = n?o ? estacion?rio: p <= 0.05
pp <- ur.pp(passageiros3)
summary(pp)

# Primeira diferencia??o
dif_passageiros <- diff(passageiros3)


# Teste pp (Philips-Perron)

# Ho = ? estacion?ria: p > 0.05
# Ha = n?o ? estacion?ria: p <= 0.05
pp <- ur.pp(dif_passageiros)
summary(pp)


# Segunda diferencia??o
dif_passageiros2 <- diff(dif_passageiros)

pp <- ur.pp(dif_passageiros2)
summary(pp)

