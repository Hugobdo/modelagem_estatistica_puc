# Prevendo o consumo de cerveja

# ------------------------------------------
# Etapa 1 - Verificando  os dados 
cerveja <- read.csv("Consumo_cerveja.csv",sep = ";", dec = ".")
View(cerveja)


# ------------------------------------------
# Etapa 2 - Separando em base de treino e teste

#install.packages("caret", dependencies = TRUE)
library(caret)

set.seed(50)
indice_treino = createDataPartition(y=cerveja$CONSUMO_CERVEJA, p=0.7, list=FALSE)
base_treino = cerveja[indice_treino, ]
base_teste = cerveja[-indice_treino, ]
View(base_treino)
View(base_teste)

# ------------------------------------------
# Etapa 3: Explorando e Preparando os Dados

# Calculando a correlação(para verificar se existe correlação com a variável resposta)
#Valores
str(base_treino)
dados_cor<-cor(base_treino[c("TEMPERATURA_MEDIA","TEMPERATURA_MINIMA","TEMPERATURA_MAXIMA","PRECIPITACAO","CONSUMO_CERVEJA")])
dados_cor

#Gráficos
#install.packages("corrplot")
require(corrplot)

corrplot(dados_cor,tl.cex = 0.6)
corrplot(dados_cor,method="color",tl.cex = 0.5)

corrplot(dados_cor,type = "upper",tl.cex = 0.6)
corrplot(dados_cor,method="color",type = "upper",tl.cex = 0.6)

# -----------------------------
#TEMPERATURA_MEDIA está altamente correlcionada com TEMPERATURA_MINIMA e TEMPERATURA_MAXIMA 
#Vamos retirar a TEMPERATURA_MEDIA(acho que inclusive a temperatura mais alta tem mais relação com consumo de cerveja)
base_treino$TEMPERATURA_MEDIA <- NULL
dados_cor<-cor(base_treino[c("TEMPERATURA_MINIMA","TEMPERATURA_MAXIMA","PRECIPITACAO","CONSUMO_CERVEJA")])
dados_cor

corrplot(dados_cor,method="color",type = "upper",tl.cex = 0.6)

# -----------------------------
#TEMPERATURA_MINIMA E TEMPERATURA_MAXIMA estão altamente correlacionadas
#Vamos tirar TEMPERATURA_MINIMA
base_treino$TEMPERATURA_MINIMA <- NULL
dados_cor<-cor(base_treino[c("TEMPERATURA_MAXIMA","PRECIPITACAO","CONSUMO_CERVEJA")])
dados_cor

corrplot(dados_cor,method="color",type = "upper",tl.cex = 0.6)


## ----------------------------------------------------------------------
# Etapa 4: Ajustando os modelos
#Ajustando o primeiro modelo
modelo1 <- lm(CONSUMO_CERVEJA ~ ., data = base_treino)
summary(modelo1)

#Prevendo o consumo de cerveja
previsao_treino <- predict(modelo1)
View(previsao_treino)

previsao_real_treino <- cbind(previsao_treino, base_treino$CONSUMO_CERVEJA)
colnames(previsao_real_treino) <- c('Previsto','Real')
previsao_real_treino <- as.data.frame(previsao_real_treino)
View(previsao_real_treino)


#MSE
mse_treino <- mean((previsao_real_treino$Previsto - previsao_real_treino$Real)^2)

#RMSE
rmse_treino <- mse_treino^0.5
rmse_treino

# ------------------------------------------
# Etapa 4: Testando o Modelo (usando os dados de teste)

# Prevendo os gastos com Dados de teste
previsao_teste <- predict(modelo1, base_teste)
previsao_teste <- as.data.frame(previsao_teste)
View(previsao_teste)

# Visualizando valores de preditos e reais da base de teste
previsao_real_teste <- cbind(previsao_teste, base_teste$CONSUMO_CERVEJA)
colnames(previsao_real_teste) <- c('Previsto','Real')
previsao_real_teste <- as.data.frame(previsao_real_teste)
View(previsao_real_teste)

#Calculando o erro médio
#MSE
mse_teste <- mean((previsao_real_teste$Previsto - previsao_real_teste$Real)^2)

#RMSE
rmse_teste <- mse_teste^0.5
rmse_teste

