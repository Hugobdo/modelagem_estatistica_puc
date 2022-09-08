############################################
###    Transformação e Diferenciação     ###
############################################

#Transformação: busca-se melhorar o grau de normalidade
#Diferenciação:transformar dados não estacionários em estacionários

# ---------------------------------------------------------------
#EXEMPLO 1

#Transformação Logaritmica
# Série mensal
set.seed(6)
dados <- rnorm(72, 10, 1)
serie1 <- ts(dados,start = c(2015,1), end=c(2020,12), frequency=12)
print(serie1)
plot(serie1)
summary(serie1)

# NORMALIDADE
# Gráfico
qqnorm(serie1)
qqline(serie1)

# Ho = distribuição normal : p > 0.05
# Ha = distribuição != normal : p <= 0.05
shapiro.test(serie1)
hist(serie1)


# Transformação por log (Diminuir variância e melhorar normalidade)
serie2 <- log(serie1)

# NORMALIDADE
hist(serie2)

# Gráfico
qqnorm(serie2)
qqline(serie2)

# Ho = distribuição normal : p > 0.05
# Ha = distribuição != normal : p <= 0.05
shapiro.test(serie2)


# -------------------------
#Transformação Exponencial
# Transformação por raiz cúbica (quando possui dados com valor zero ou negativos)
serie3 <- (serie1)^(1/3)

# NORMALIDADE
hist(serie3)

# Gráfico
qqnorm(serie3)
qqline(serie3)

# Ho = distribuição normal : p > 0.05
# Ha = distribuição != normal : p <= 0.05
shapiro.test(serie3)


# ------------------------
## Diferenciação
# Estacionaridade

library(forecast)
decomposicao <- decompose(serie2)
autoplot(decomposicao)

# Teste pp (Philips-Perron)

# Ho = é estacionária: p > 0.05
# Ha = não é estacionária: p <= 0.05
library("urca")
pp <- ur.pp(serie2)
summary(pp)

# Indicação de quantas diferenciações devem ser feitas
ndiffs(serie2)



# ---------------------------------------------------------------
#EXEMPLO 2
# Número de passageiros aéreos entre 1949 a 1960
passageiros <- AirPassengers
plot(passageiros)


# NORMALIDADE

# Gráfico
qqnorm(passageiros)
qqline(passageiros)

# Ho = distribuição normal : p > 0.05
# Ha = distribuição != normal : p <= 0.05
shapiro.test(passageiros)
hist(passageiros)

#------------------------
# Transformação (Diminuir variância e melhorar normalidade)

#Logaritmica
passageiros2 <- log(passageiros)
hist(passageiros2)
shapiro.test(passageiros2)

#Raiz Cúbica
passageiros3 <- passageiros^(1/3)
hist(passageiros3)
shapiro.test(passageiros3)


qqnorm(passageiros2)
qqline(passageiros2)

qqnorm(passageiros3)
qqline(passageiros3)


#------------------------
## Diferenciação
# Estacionaridade

library(forecast)
decomposicao <- decompose(passageiros3)
autoplot(decomposicao)

# Indicação de quantas diferenciações devem ser feitas
ndiffs(passageiros3)
#Se uma não deixar a série estacionária, não adianta fazer outra

# Teste pp (Philips-Perron)

# Ho = é estacionário: p > 0.05
# Ha = não é estacionário: p <= 0.05
pp <- ur.pp(passageiros3)
summary(pp)

# Primeira diferenciação
dif_passageiros <- diff(passageiros3)


# Teste pp (Philips-Perron)

# Ho = é estacionária: p > 0.05
# Ha = não é estacionária: p <= 0.05
pp <- ur.pp(dif_passageiros)
summary(pp)


# Segunda diferenciação
dif_passageiros2 <- diff(dif_passageiros)

pp <- ur.pp(dif_passageiros2)
summary(pp)

