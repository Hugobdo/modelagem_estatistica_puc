#################################################
###    TRATAMENTO DOS DADOS: PROJETO CHUVA    ###
#################################################

#### INFORMA??ES DO DATASET

# http://www.hidrologia.daee.sp.gov.br/
# PREFIXO: 	E3-262	
# NOME DO POSTO: 	GUARAU	
# MUNIC?PIO: 	SAO PAULO	
# CURSO D'?GUA: 	CABUCU DE BAIXO,R/(ITAGUACU,COR)	
# LATITUDE: 	23?26'33"	
# LONGITUDE: 	46?39'01"	
# DADOS DE 1985 A 2021

# CARREGAR PACOTE
library(dplyr)

# ABRIR ARQUIVO
chuva <- read.csv('chuva_mensal.csv', sep = ";", encoding = "UTF-8")
View(chuva)

str(chuva)

chuva$Janeiro <- as.numeric(chuva$Janeiro)
chuva$Fevereiro <- as.numeric(chuva$Fevereiro)
chuva$Mar?o <- as.numeric(chuva$Mar?o)
chuva$Maio <- as.numeric(chuva$Maio)
chuva$Junho <- as.numeric(chuva$Junho)
chuva$Julho <- as.numeric(chuva$Julho)
chuva$Agosto <- as.numeric(chuva$Agosto)
chuva$Setembro <- as.numeric(chuva$Setembro)
chuva$Outubro <- as.numeric(chuva$Outubro)
chuva$Novembro <- as.numeric(chuva$Novembro)
chuva$Dezembro <- as.numeric(chuva$Dezembro)


# Renomeando vari?veis (colunas)
chuva <- rename(chuva, ano = X.U.FEFF.Ano)
View(chuva)

str(chuva)

# Verificando valores missing (Ausentes)
# NA = valores ausentes
# NAN = not a number(valor indefinido)
sapply(chuva, function(x) sum(is.na(x)))


#Substituir valores missing

chuva$Janeiro[is.na(chuva$Janeiro)] <- mean(chuva$Janeiro[which(chuva$Janeiro!='NA')])
chuva$Fevereiro[is.na(chuva$Fevereiro)] <- mean(chuva$Fevereiro[which(chuva$Fevereiro!='NA')])
chuva$Mar?o[is.na(chuva$Mar?o)] <- mean(chuva$Mar?o[which(chuva$Mar?o!='NA')])
chuva$Maio[is.na(chuva$Maio)] <- mean(chuva$Maio[which(chuva$Maio!='NA')])
chuva$Junho[is.na(chuva$Junho)] <- mean(chuva$Junho[which(chuva$Junho!='NA')])
chuva$Julho[is.na(chuva$Julho)] <- mean(chuva$Julho[which(chuva$Julho!='NA')])
chuva$Agosto[is.na(chuva$Agosto)] <- mean(chuva$Agosto[which(chuva$Agosto!='NA')])
chuva$Setembro[is.na(chuva$Setembro)] <- mean(chuva$Setembro[which(chuva$Setembro!='NA')])
chuva$Outubro[is.na(chuva$Outubro)] <- mean(chuva$Outubro[which(chuva$Outubro!='NA')])
chuva$Novembro[is.na(chuva$Novembro)] <- mean(chuva$Novembro[which(chuva$Novembro!='NA')])
chuva$Dezembro[is.na(chuva$Dezembro)] <- mean(chuva$Dezembro[which(chuva$Dezembro!='NA')])

sapply(chuva, function(x) sum(is.na(x)))
str(chuva)

#Excluir Linhas 
#Vou utilizar o ano de 2021 para testar nosso modelo, por isso vou excluir do banco de dados
chuva2 <- chuva %>% filter(ano!=2021)
#Retirar a linha de m?dia
chuva2 <- chuva2 %>% filter(ano!="Media")
View(chuva2)

# EXCLUIR UMA COLUNA
chuva2 <- subset(chuva2, select = -c(ano))

# Exporta??o de arquivos
write.table(chuva2, file ="chuva_tratado.csv", sep = ";")

###########################
###      AN?LISE         ###
###########################

# CARREGAR PACOTES
library(dplyr)

# ABRIR ARQUIVO
chuva_sp <- read.csv('chuva_tratado.csv', sep = ";")
View(chuva_sp)

str(chuva_sp)

#O banco n?o pode ser data frame, tem que ser vetor
chuva_sp2 <- as.vector(t(chuva_sp))
print(chuva_sp2)

##### Cria??o da s?rie #####
serie <- ts(chuva_sp2, start = c(1985,1), end = c(2020,12), frequency=12)
print(serie)
plot(serie)

## ---------------------
### DECOMPOSI??O
decomposicao <- decompose(serie)
plot(decomposicao, col = "brown")

#Tend?ncia
plot(decomposicao$trend)
#Sazonalidade
plot(decomposicao$seasonal)
#residuo
plot(decomposicao$random)
#Apenas um intervalo espec?fico
plot(window(decomposicao$trend, start=2011, end=2013))

#efeito sazonal por ano
library("forecast")
ggseasonplot(window(serie, start=c(2011), end=2016))

## ---------------------
### SUAVIZA??O - para tentar remover algum outlier
serie2 <- tsclean(serie)

# Compara??o
plot(serie)
lines(serie2, col="red")

## ---------------------
### NORMALIDADE
qqnorm(serie2)
qqline(serie2)

# Ho = distribui??o normal : p > 0.05
# Ha = distribui??o != normal : p <= 0.05
shapiro.test(serie2)

## ---------------------
### TRANSFORMA??O
# Para melhorar a normalidade
# Vamos verificar se tem valores negativos ou zero
#Se tiver n?o podemos utilizar a transforma??o logaritmica
summary(serie2)

serie3 <- (serie2)^(1/3)

hist(serie2)
hist(serie3)

qqnorm(serie3)
qqline(serie3)

# Ho = distribui??o normal : p > 0.05
# Ha = distribui??o != normal : p <= 0.05
shapiro.test(serie3)


## ---------------------
#### ESTACIONARIDADE
library("urca")
# Teste pp (Philips-Perron)

# Ho = ? estacion?ria: p > 0.05
# Ha = n?o ? estacion?ria: p <= 0.05
estacionaridade <- ur.pp(serie3)
summary(estacionaridade)

ndiffs(serie3)

serie4 <- diff(serie3)
estacionaridade = ur.pp(serie4)
summary(estacionaridade)

serie5 <- diff(serie4)
estacionaridade <- ur.pp(serie5)
summary(estacionaridade)

#mas quando observamos a s?rie, nota-se que ela aparenta ser estacion?ria
plot(serie3)

# Teste KPSS (Kwiatkowski-Phillips-Schmidt-Shin)

# Ho = n?o ? estacion?rio: teste estat?stico > valor cr?tico
# Ha = ? estacion?rio:  teste estat?stico < valor cr?tico
kpss <- ur.kpss(serie3)
summary(kpss)

# Teste df (Dickey Fuller)

# Ho = n?o ? estacion?rio: teste estat?stico > valor cr?tico
# Ha = ? estacion?rio:  teste estat?stico < valor cr?tico
df <- ur.df(serie3)
summary(df)

## ---------------------
#### AUTOCORRELA??O
acf(serie3)
#Mostra que a s?rie tem uma sazonalidade(pela forma) e que tem autocorrela??o
#Por enquanto n?o tem problema. Problema ser? apenas quando formos analisar os res?duos
pacf(serie3)
tsdisplay(serie3)

# Teste de Autocorrela??o (Ljung-Box)
# Ho = n?o ? autocorrelacionado: p > 0.05
# Ha = ? autocorrelacionado: p <= 0.05
Box.test(serie3, type = "Ljung-Box")


##########################################################################
#### MODELO AR

# modelo arima:(p,d,q)
# modelo ar: (p,0,0)
acf(serie3)
modelo_ar <- arima(serie3, order = c(1,0,0))
summary(modelo_ar)

# An?lise dos res?duos (qualidade do modelo)
checkresiduals(modelo_ar)

mean(resid(modelo_ar))
#A m?dia est? em torno do 0 e a vari?ncia deve estar constante
# Tem autocorrela??o

plot(resid(modelo_ar))

qqnorm(resid(modelo_ar))
qqline(resid(modelo_ar))

# Ho = distribui??o normal : p > 0.05
# Ha = distribui??o != normal : p <= 0.05
shapiro.test(resid(modelo_ar))

acf(resid(modelo_ar))
pacf(resid(modelo_ar))

plot(serie3)
lines(serie3-modelo_ar$resid, col= "red")

#Previs?o de 3 anos
previsao <- forecast(modelo_ar,h=36)
plot(previsao)
lines(serie3-modelo_ar$resid, col= "red")

print(previsao)

#Voltando com a escala
prev_escala <- as.data.frame(previsao)^3
View(prev_escala)


#########################
# Ao verificar a previs?o observamos que ela n?o est? muito boa.
#Temos que ajustar os par?metros do modelo
acf(serie3)
modelo_ar4 <- arima(serie3, order = c(4,0,0))
summary(modelo_ar4)

# An?lise dos res?duos (qualidade do modelo)
checkresiduals(modelo_ar4)

mean(resid(modelo_ar4))
#A m?dia est? em torno do 0 e a vari?ncia deve estar constante
# Tem autocorrela??o

plot(resid(modelo_ar4))

qqnorm(resid(modelo_ar4))
qqline(resid(modelo_ar4))

# Ho = distribui??o normal : p > 0.05
# Ha = distribui??o != normal : p <= 0.05
shapiro.test(resid(modelo_ar4))

acf(resid(modelo_ar4))
pacf(resid(modelo_ar4))

plot(serie3)
lines(serie3-modelo_ar4$resid, col= "red")

previsao4 <- forecast(modelo_ar4,h=36)
plot(previsao4)
lines(serie3-modelo_ar4$resid, col= "red")

print(previsao)

prev_escala4 <- as.data.frame(previsao4)^3
View(prev_escala4)

#########################
# Ao verificar a previs?o observamos que ela n?o est? muito boa.
#Temos que ajustar os par?metros do modelo
acf(serie3)
modelo_ar10 <- arima(serie3, order = c(10,0,0))
summary(modelo_ar10)

# An?lise dos res?duos (qualidade do modelo)
checkresiduals(modelo_ar10)

#A m?dia est? em torno do 0 e a vari?ncia deve estar constante
summary(resid(modelo_ar10))

plot(resid(modelo_ar10))

#Normalidade
qqnorm(resid(modelo_ar10))
qqline(resid(modelo_ar10))

# Ho = distribui??o normal : p > 0.05
# Ha = distribui??o != normal : p <= 0.05
shapiro.test(resid(modelo_ar10))

#Autocorrela??o
acf(resid(modelo_ar10))
pacf(resid(modelo_ar10))

plot(serie3)
lines(serie3-modelo_ar10$resid, col= "red")

previsao10 <- forecast(modelo_ar10,h=36)
plot(previsao10)
lines(serie3-modelo_ar10$resid, col= "red")

prev_escala10 <- as.data.frame(previsao10)^3
View(prev_escala10)



##########################################################################
#### MODELO MA
# modelo arima:(p,d,q)
# modelo ma: (0,0,q)
modelo_ma <- arima(serie3, order = c(0,0,1))
summary(modelo_ma)
#AIC muito alto - 1525.33

modelo_ma <- arima(serie3, order = c(0,0,4))
summary(modelo_ma)
#AIC muito alto - 1475.07

modelo_ma <- arima(serie3, order = c(0,0,6))
summary(modelo_ma)
#AIC muito alto - 1436.89

modelo_ma <- arima(serie3, order = c(0,0,8))
summary(modelo_ma)
#AIC muito alto - 1397.16

modelo_ma <- arima(serie3, order = c(0,0,10))
summary(modelo_ma)
#AIC muito alto - 1400.05


modelo_ma <- arima(serie3, order = c(0,0,8))
summary(modelo_ma)

# An?lise dos res?duos (qualidade do modelo)
checkresiduals(modelo_ma)

plot(resid(modelo_ma))

qqnorm(resid(modelo_ma))
qqline(resid(modelo_ma))

# Ho = distribui??o normal : p > 0.05
# Ha = distribui??o != normal : p <= 0.05
shapiro.test(resid(modelo_ma))

acf(resid(modelo_ma))
pacf(resid(modelo_ma))

plot(serie3)
lines(serie3-modelo_ma$resid, col= "red")

previsao2 <- forecast(modelo_ma,h=24)
plot(previsao2)
lines(serie3-modelo_ma$resid, col= "red")

print(previsao2)

prev_escala2 <- as.data.frame(previsao2)^3
View(prev_escala2)




##########################################################################
# Junato o AR + MA

#### MODELO ARMA
# modelo arima:(p,d,q)
# modelo ma: (p,0,q)

modelo_arma <- arima(serie3, order = c(10,0,8))
summary(modelo_arma)

# An?lise dos res?duos (qualidade do modelo)
checkresiduals(modelo_arma)

plot(resid(modelo_arma))

qqnorm(resid(modelo_arma))
qqline(resid(modelo_arma))

# Ho = distribui??o normal : p > 0.05
# Ha = distribui??o != normal : p <= 0.05
shapiro.test(resid(modelo_arma))

acf(resid(modelo_arma))
pacf(resid(modelo_arma))

plot(serie3)
lines(serie3-modelo_arma$resid, col= "red")

previsao3 <- forecast(modelo_arma,h=24)
plot(previsao3)
lines(serie3-modelo_arma$resid, col= "red")

print(previsao3)

prev_escala3 <- as.data.frame(previsao3)^3
View(prev_escala3)


##########################################################################
#### MODELO ARIMA
ndiffs(serie3)
modelo_arima <- arima(serie3, order = c(10,1,8))
summary(modelo_arima)

# An?lise dos res?duos (qualidade do modelo)
checkresiduals(modelo_arima)

plot(resid(modelo_arima))

summary(resid(modelo_arima))

qqnorm(resid(modelo_arima))
qqline(resid(modelo_arima))

# Ho = distribui??o normal : p > 0.05
# Ha = distribui??o != normal : p <= 0.05
shapiro.test(resid(modelo_arima))

acf(resid(modelo_arima))
pacf(resid(modelo_arima))

plot(serie3)
lines(serie3-modelo_arima$resid, col= "red")

previsao4 <- forecast(modelo_arima,h=24)
plot(previsao4)
lines(serie3-modelo_arima$resid, col= "red")

print(previsao4)

prev_escala4 <- as.data.frame(previsao4)^3
View(prev_escala4)


##########################################################################
#### MODELO SARIMA: (p,d,q)(P,D,Q)

modelo_sarima <- arima(serie3, order = c(8,0,8), seasonal = c(1,0,1))
summary(modelo_sarima)

# An?lise dos res?duos (qualidade do modelo)
checkresiduals(modelo_sarima)

plot(resid(modelo_sarima))

qqnorm(resid(modelo_sarima))
qqline(resid(modelo_sarima))

# Ho = distribui??o normal : p > 0.05
# Ha = distribui??o != normal : p <= 0.05
shapiro.test(resid(modelo_sarima))

acf(resid(modelo_sarima))
pacf(resid(modelo_sarima))

plot(serie3)
lines(serie3-modelo_sarima$resid, col= "red")

previsao5 <- forecast(modelo_sarima,h=24)
plot(previsao5)
lines(serie3-modelo_sarima$resid, col= "red")

print(previsao5)

prev_escala5 <- as.data.frame(previsao5)^3
View(prev_escala5)

##########################################################################
### AUTOARIMA
?auto.arima
modelo_auto <- auto.arima(serie3, trace = T, stepwise = F, approximation = F,
                          max.p = 5, max.q = 5, max.P = 2, max.Q = 2)

# Trace: apresenta no console a lista dos modelos.
# stepwise: sele??o gradual(processo mais r?pido, por?m menos minucioso)
# approximation: sele??o do melhor modelo por aproxima??o
#           (indicado para s?ries muito longas, diminui tempo computacional)
# Drift do modelo ? um par?metro que representa a tend?ncia temporal num passeio aleat?rio.
# Interessante dobrar as ordens m?ximas: max.p = 10, max.q = 10, max.P = 4, max.Q = 4
summary(modelo_auto)


# An?lise dos res?duos (qualidade do modelo)
checkresiduals(modelo_auto)

plot(resid(modelo_auto))

qqnorm(resid(modelo_auto))
qqline(resid(modelo_auto))

# Ho = distribui??o normal : p > 0.05
# Ha = distribui??o != normal : p <= 0.05
shapiro.test(resid(modelo_auto))

acf(resid(modelo_auto))
pacf(resid(modelo_auto))

plot(serie3)
lines(serie3-modelo_auto$resid, col= "red")

previsao6 <- forecast(modelo_auto,h=24)
plot(previsao6)
lines(serie3-modelo_auto$resid, col= "red")

prev_escala6 <- as.data.frame(previsao6)^3
View(prev_escala6)


## Dobrando os par?metros
modelo_auto <- auto.arima(serie3, trace = T, stepwise = F, approximation = F,
                          max.p = 10, max.q = 10, max.P = 4, max.Q = 4)
summary(modelo_auto)


# An?lise dos res?duos (qualidade do modelo)
checkresiduals(modelo_auto)

plot(resid(modelo_auto))

qqnorm(resid(modelo_auto))
qqline(resid(modelo_auto))

# Ho = distribui??o normal : p > 0.05
# Ha = distribui??o != normal : p <= 0.05
shapiro.test(resid(modelo_auto))

acf(resid(modelo_auto))
pacf(resid(modelo_auto))

plot(serie3)
lines(serie3-modelo_auto$resid, col= "red")

previsao6 <- forecast(modelo_auto,h=24)
plot(previsao6)
lines(serie3-modelo_auto$resid, col= "red")

prev_escala6 <- as.data.frame(previsao6)^3
View(prev_escala6)
