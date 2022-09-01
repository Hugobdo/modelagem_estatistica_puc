## Carregando os dados
setwd("C:/Users/Desktop/Arquivos/PUC - Turma 4/Exemplos em Sala/Regressão Simples")
library(readxl)
dados_regressao_simples <- read_excel("REGRESSAO_SIMPLES_Housing.xlsx")
View(dados_regressao_simples)

# ------------------------------------------
# Etapa 1 - Separando em base de treino e teste

#install.packages("caret", dependencies = TRUE)
library(caret)
set.seed(40)
indice_treino = createDataPartition(y=dados_regressao_simples$TX_OCUPACAO, p=0.7, list=FALSE)
base_treino = dados_regressao_simples[indice_treino, ]
base_teste = dados_regressao_simples[-indice_treino, ]
View(base_treino)
View(base_teste)


## ----------------------------------------------------------------------
## Etapa 2 - Calculando a correlação

#Correlação
cor.test(base_treino$TX_OCUPACAO,base_treino$MEDIA_QUARTOS,method='pearson')

## ----------------------------------------------------------------------
## Etapa 3 - Diagrama de Dispersão
plot(base_treino$MEDIA_QUARTOS, 
     base_treino$TX_OCUPACAO,
     main="Dispersão entre média de quartos e taxa de ocupação",
     xlab="Média Quartos", 
     ylab="Taxa Ocupação",
     col="lightskyblue3",
     pch=20)

## ----------------------------------------------------------------------
## Etapa 4 - Diagrama de Dispersão
#Criando modelo de regressão
#Ajuste do modelo
modelo <- lm(TX_OCUPACAO ~ MEDIA_QUARTOS, data = base_treino)
summary(modelo)

#Prevendo a taxa de ocupação
previsao_treino <- predict(modelo,base_treino)
View(previsao_treino)

previsao_real_treino <- cbind(previsao_treino, base_treino$TX_OCUPACAO)
colnames(previsao_real_treino) <- c('Previsto','Real')
previsao_real_treino <- as.data.frame(previsao_real_treino)
View(previsao_real_treino)


#MSE
mse_treino <- mean((previsao_real_treino$Previsto - previsao_real_treino$Real)^2)

#RMSE
rmse_treino <- mse_treino^0.5
rmse_treino


## ----------------------------------------------------------------------
## Fazendo a reta de regressão no gráfico
abline(lm(base_treino$TX_OCUPACAO~base_treino$MEDIA_QUARTOS), 
       col="red",
       lwd = 2)

# ------------------------------------------
# Etapa 5: Testando o Modelo (usando os dados de teste)

# Prevendo os gastos com Dados de teste
previsao_teste <- predict(modelo, base_teste)
previsao_teste <- as.data.frame(previsao_teste)
View(previsao_teste)

# Visualizando valores de preditos e reais da base de teste
previsao_real_teste <- cbind(previsao_teste, base_teste$TX_OCUPACAO)
colnames(previsao_real_teste) <- c('Previsto','Real')
previsao_real_teste <- as.data.frame(previsao_real_teste)
View(previsao_real_teste)

#Calculando o erro médio
#MSE
mse_teste <- mean((previsao_real_teste$Previsto - previsao_real_teste$Real)^2)

#RMSE
rmse_teste <- mse_teste^0.5
rmse_teste


## ----------------------------------------------------------------------
# Fazendo previsões
class(base_teste)
class(base_treino)


MEDIA_QUARTOS <- 7
NOVO_VALOR <- data.frame(MEDIA_QUARTOS)
class(NOVO_VALOR)

PREVISAO_NOVA_TAXA <- predict(modelo,newdata = NOVO_VALOR, type = "response")
PREVISAO_NOVA_TAXA



## ----------------------------------------------------------------------
# Para verificar o que existe dentro do ajuste do modelo
is.list(modelo)
ls(modelo)
modelo$coefficients







