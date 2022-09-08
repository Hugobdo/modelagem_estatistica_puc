# ----------------------------------------------------------------#
# Criação de uma série etmporal aleatória com distribuição Normal #
# ----------------------------------------------------------------#


## Série Anual

set.seed(10) 

#Dados aleatórios com distribuição Normal Usa 41 porque o intervalo de tempos são 41 anos
dados1 <- rnorm(41) 

#frequencia anual coloco = 1
#Mensal colocaria 12
serie1 <- ts(dados1, start = c(1980), end=c(2020),frequency = 1)

plot(serie1)
print(serie1)

## Série mensal
dados2<-rnorm(72)
serie2 <- ts(dados2, start = c(2015,1), end=c(2020,12),frequency = 12)
print(serie2)
plot(serie2)

## Série diaria
dados3<-rnorm(730)
serie3 <- ts(dados3, start = c(2019,1,1),frequency = 365.25)
print(serie3)
plot(serie3)

## Série trimestral
dados4<-rnorm(164)
serie4 <- ts(dados4, start = 1980, end = 2020, frequency = 4)
print(serie4)
plot(serie4)
