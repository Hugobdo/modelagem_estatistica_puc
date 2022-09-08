#################################################
###    TRATAMENTO DOS DADOS: PROJETO CHUVA    ###
#################################################

#### INFORMAÇÕES DO DATASET

# http://www.hidrologia.daee.sp.gov.br/
# PREFIXO: 	E3-262	
# NOME DO POSTO: 	GUARAU	
# MUNICÍPIO: 	SAO PAULO	
# CURSO D'ÁGUA: 	CABUCU DE BAIXO,R/(ITAGUACU,COR)	
# LATITUDE: 	23°26'33"	
# LONGITUDE: 	46°39'01"	
# DADOS DE 1985 A 2021

# CARREGAR PACOTE
library(dplyr)

# ABRIR ARQUIVO
chuva <- read.csv('chuva_mensal.csv', sep = ";", encoding = "UTF-8")
View(chuva)

str(chuva)

chuva$Janeiro <- as.numeric(chuva$Janeiro)
chuva$Fevereiro <- as.numeric(chuva$Fevereiro)
chuva$Março <- as.numeric(chuva$Março)
chuva$Maio <- as.numeric(chuva$Maio)
chuva$Junho <- as.numeric(chuva$Junho)
chuva$Julho <- as.numeric(chuva$Julho)
chuva$Agosto <- as.numeric(chuva$Agosto)
chuva$Setembro <- as.numeric(chuva$Setembro)
chuva$Outubro <- as.numeric(chuva$Outubro)
chuva$Novembro <- as.numeric(chuva$Novembro)
chuva$Dezembro <- as.numeric(chuva$Dezembro)


# Renomeando variáveis (colunas)
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
chuva$Março[is.na(chuva$Março)] <- mean(chuva$Março[which(chuva$Março!='NA')])
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
#Retirar a linha de média
chuva2 <- chuva2 %>% filter(ano!="Media")
View(chuva2)

# EXCLUIR UMA COLUNA
chuva2 <- subset(chuva2, select = -c(ano))

# Exportação de arquivos
write.table(chuva2, file ="chuva_tratado.csv", sep = ";")

###########################
###      ANÁLISE         ###
###########################

# CARREGAR PACOTES
library(dplyr)

# ABRIR ARQUIVO
chuva_sp <- read.csv('chuva_tratado.csv', sep = ";")
View(chuva_sp)

str(chuva_sp)

#O banco não pode ser data frame, tem que ser vetor
chuva_sp2 <- as.vector(t(chuva_sp))
print(chuva_sp2)

##### Criação da série #####
serie <- ts(chuva_sp2, start = c(1985,1), end = c(2020,12), frequency=12)
print(serie)
plot(serie)

## ---------------------
### DECOMPOSIÇÃO
decomposicao <- decompose(serie)
plot(decomposicao, col = "brown")

#Tendência
plot(decomposicao$trend)
#Sazonalidade
plot(decomposicao$seasonal)
#residuo
plot(decomposicao$random)
#Apenas um intervalo específico
plot(window(decomposicao$trend, start=2011, end=2013))

#efeito sazonal por ano
library("forecast")
ggseasonplot(window(serie, start=c(2011), end=2016))

## ---------------------
### SUAVIZAÇÃO - para tentar remover algum outlier
serie2 <- tsclean(serie)

# Comparação
plot(serie)
lines(serie2, col="red")

## ---------------------
### NORMALIDADE
qqnorm(serie2)
qqline(serie2)

# Ho = distribuição normal : p > 0.05
# Ha = distribuição != normal : p <= 0.05
shapiro.test(serie2)

## ---------------------
### TRANSFORMAÇÃO
# Para melhorar a normalidade
# Vamos verificar se tem valores negativos ou zero
#Se tiver não podemos utilizar a transformação logaritmica
summary(serie2)

serie3 <- (serie2)^(1/3)

hist(serie2)
hist(serie3)

qqnorm(serie3)
qqline(serie3)

# Ho = distribuição normal : p > 0.05
# Ha = distribuição != normal : p <= 0.05
shapiro.test(serie3)


## ---------------------
#### ESTACIONARIDADE
library("urca")
# Teste pp (Philips-Perron)

# Ho = é estacionária: p > 0.05
# Ha = não é estacionária: p <= 0.05
estacionaridade <- ur.pp(serie3)
summary(estacionaridade)

ndiffs(serie3)

serie4 <- diff(serie3)
estacionaridade = ur.pp(serie4)
summary(estacionaridade)

serie5 <- diff(serie4)
estacionaridade <- ur.pp(serie5)
summary(estacionaridade)

#mas quando observamos a série, nota-se que ela aparenta ser estacionária
plot(serie3)

# Teste KPSS (Kwiatkowski-Phillips-Schmidt-Shin)

# Ho = não é estacionário: teste estatístico > valor crítico
# Ha = é estacionário:  teste estatístico < valor crítico
kpss <- ur.kpss(serie3)
summary(kpss)

# Teste df (Dickey Fuller)

# Ho = não é estacionário: teste estatístico > valor crítico
# Ha = é estacionário:  teste estatístico < valor crítico
df <- ur.df(serie3)
summary(df)

## ---------------------
#### AUTOCORRELAÇÃO
acf(serie3)
#Mostra que a série tem uma sazonalidade(pela forma) e que tem autocorrelação
#Por enquanto não tem problema. Problema será apenas quando formos analisar os resíduos
pacf(serie3)
tsdisplay(serie3)

# Teste de Autocorrelação (Ljung-Box)
# Ho = não é autocorrelacionado: p > 0.05
# Ha = é autocorrelacionado: p <= 0.05
Box.test(serie3, type = "Ljung-Box")


##########################################################################
#### MODELO AR

# modelo arima:(p,d,q)
# modelo ar: (p,0,0)
acf(serie3)
modelo_ar <- arima(serie3, order = c(1,0,0))
summary(modelo_ar)

# Análise dos resíduos (qualidade do modelo)
checkresiduals(modelo_ar)

mean(resid(modelo_ar))
#A média está em torno do 0 e a variância deve estar constante
# Tem autocorrelação

plot(resid(modelo_ar))

qqnorm(resid(modelo_ar))
qqline(resid(modelo_ar))

# Ho = distribuição normal : p > 0.05
# Ha = distribuição != normal : p <= 0.05
shapiro.test(resid(modelo_ar))

acf(resid(modelo_ar))
pacf(resid(modelo_ar))

plot(serie3)
lines(serie3-modelo_ar$resid, col= "red")

#Previsão de 3 anos
previsao <- forecast(modelo_ar,h=36)
plot(previsao)
lines(serie3-modelo_ar$resid, col= "red")

print(previsao)

#Voltando com a escala
prev_escala <- as.data.frame(previsao)^3
View(prev_escala)


#########################
# Ao verificar a previsão observamos que ela não está muito boa.
#Temos que ajustar os parâmetros do modelo
acf(serie3)
modelo_ar4 <- arima(serie3, order = c(4,0,0))
summary(modelo_ar4)

# Análise dos resíduos (qualidade do modelo)
checkresiduals(modelo_ar4)

mean(resid(modelo_ar4))
#A média está em torno do 0 e a variância deve estar constante
# Tem autocorrelação

plot(resid(modelo_ar4))

qqnorm(resid(modelo_ar4))
qqline(resid(modelo_ar4))

# Ho = distribuição normal : p > 0.05
# Ha = distribuição != normal : p <= 0.05
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
# Ao verificar a previsão observamos que ela não está muito boa.
#Temos que ajustar os parâmetros do modelo
acf(serie3)
modelo_ar10 <- arima(serie3, order = c(10,0,0))
summary(modelo_ar10)

# Análise dos resíduos (qualidade do modelo)
checkresiduals(modelo_ar10)

#A média está em torno do 0 e a variância deve estar constante
summary(resid(modelo_ar10))

plot(resid(modelo_ar10))

#Normalidade
qqnorm(resid(modelo_ar10))
qqline(resid(modelo_ar10))

# Ho = distribuição normal : p > 0.05
# Ha = distribuição != normal : p <= 0.05
shapiro.test(resid(modelo_ar10))

#Autocorrelação
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

# Análise dos resíduos (qualidade do modelo)
checkresiduals(modelo_ma)

plot(resid(modelo_ma))

qqnorm(resid(modelo_ma))
qqline(resid(modelo_ma))

# Ho = distribuição normal : p > 0.05
# Ha = distribuição != normal : p <= 0.05
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

# Análise dos resíduos (qualidade do modelo)
checkresiduals(modelo_arma)

plot(resid(modelo_arma))

qqnorm(resid(modelo_arma))
qqline(resid(modelo_arma))

# Ho = distribuição normal : p > 0.05
# Ha = distribuição != normal : p <= 0.05
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

# Análise dos resíduos (qualidade do modelo)
checkresiduals(modelo_arima)

plot(resid(modelo_arima))

summary(resid(modelo_arima))

qqnorm(resid(modelo_arima))
qqline(resid(modelo_arima))

# Ho = distribuição normal : p > 0.05
# Ha = distribuição != normal : p <= 0.05
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

# Análise dos resíduos (qualidade do modelo)
checkresiduals(modelo_sarima)

plot(resid(modelo_sarima))

qqnorm(resid(modelo_sarima))
qqline(resid(modelo_sarima))

# Ho = distribuição normal : p > 0.05
# Ha = distribuição != normal : p <= 0.05
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
# stepwise: seleção gradual(processo mais rápido, porém menos minucioso)
# approximation: seleção do melhor modelo por aproximação
#           (indicado para séries muito longas, diminui tempo computacional)
# Drift do modelo é um parâmetro que representa a tendência temporal num passeio aleatório.
# Interessante dobrar as ordens máximas: max.p = 10, max.q = 10, max.P = 4, max.Q = 4
summary(modelo_auto)


# Análise dos resíduos (qualidade do modelo)
checkresiduals(modelo_auto)

plot(resid(modelo_auto))

qqnorm(resid(modelo_auto))
qqline(resid(modelo_auto))

# Ho = distribuição normal : p > 0.05
# Ha = distribuição != normal : p <= 0.05
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


## Dobrando os parâmetros
modelo_auto <- auto.arima(serie3, trace = T, stepwise = F, approximation = F,
                          max.p = 10, max.q = 10, max.P = 4, max.Q = 4)
summary(modelo_auto)


# Análise dos resíduos (qualidade do modelo)
checkresiduals(modelo_auto)

plot(resid(modelo_auto))

qqnorm(resid(modelo_auto))
qqline(resid(modelo_auto))

# Ho = distribuição normal : p > 0.05
# Ha = distribuição != normal : p <= 0.05
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
