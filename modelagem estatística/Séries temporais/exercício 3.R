############################################################
####    SELE??O DO MELHOR MODELO E AN?LISE DOS ERROS    ####
############################################################

# CARREGAR PACOTES
library(dplyr)
library(readxl)

# ABRIR ARQUIVO
chuva_sp <- read_excel('EXERCICIO3_Inflacao.xlsx')
View(chuva_sp)
chuva_sp <- subset(chuva_sp, select = -c(ANO))

#Excluir Linhas
chuva_sp2 <- chuva_sp[-13, ]
View(chuva_sp2) 
str(chuva_sp2)

chuva_sp3 <- as.vector(t(chuva_sp2))
print(chuva_sp3)


##### Cria??o da s?rie #####
serie <- ts(chuva_sp3, start = c(1985,1), end = c(2019,12), frequency=12)
print(serie)
plot(serie)


### DECOMPOSI??O
decomposicao <- decompose(serie)
plot(decomposicao)

### SUAVIZA??O
library("forecast")
serie2 <- tsclean(serie)

# Compara??o
plot(serie)
lines(serie2, col="red")


### NORMALIDADE
qqnorm(serie2)
qqline(serie2)

# Ho = distribui??o normal : p > 0.05
# Ha = distribui??o != normal : p <= 0.05
shapiro.test(serie2)

### TRANSFORMA??O
serie3 <- (serie2)^(1/3)
print(serie3)

hist(serie2)
hist(serie3)

qqnorm(serie3)
qqline(serie3)

shapiro.test(serie3)

#### ESTACIONARIDADE
library("urca")
# Teste pp (Philips-Perron)

# Ho = ? estacion?ria: p > 0.05
# Ha = n?o ? estacion?ria: p <= 0.05
estacionaridade <- ur.pp(serie3)
summary(estacionaridade)

ndiffs(serie3)


#### AUTOCORRELA??O
tsdisplay(serie3)

# Teste de Autocorrela??o (Ljung-Box)
# Ho = n?o ? autocorrelacionado: p > 0.05
# Ha = ? autocorrelacionado: p <= 0.05
Box.test(serie3, type = "Ljung-Box")



#### MODELO auto.ARIMA: (p,d,q)(P,D,Q)

modelo_auto <- auto.arima(serie3, trace = T, stepwise = F, approximation = F,
                          max.p = 5, max.q = 5, max.P = 2, max.Q = 2)

summary(modelo_auto)

modelo_auto2 <- auto.arima(serie3, trace = T, stepwise = F, approximation = F,
                          max.p = 10, max.q = 10, max.P = 4, max.Q = 4)

summary(modelo_auto2)
