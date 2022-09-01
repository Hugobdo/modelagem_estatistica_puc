# Prevendo utilização de bicicleta

# ------------------------------------------
# Etapa 1 - Verificando  os dados 
bike <- read.csv("SeoulBikeData.csv",sep = ";", dec = ".")
View(bike)

# ------------------------------------------
# Etapa 2 - Separando em base de treino e teste

#install.packages("caret", dependencies = TRUE)
library(caret)

set.seed(50)
indice_treino = createDataPartition(y=bike$BICICLETAS_ALUGADAS, p=0.7, list=FALSE)
base_treino = bike[indice_treino, ]
base_teste = bike[-indice_treino, ]
View(base_treino)
View(base_teste)

# ------------------------------------------
# Etapa 3: Explorando e Preparando os Dados

# Calculando a correlação(para verificar se existe correlação com a variável resposta)
#Valores
str(base_treino)
dados_cor<-cor(base_treino[c("HORA","TEMPERATURA","UMIDADE","VELOCIDADE_VENTO","VISIBILIDADE","RADIACAO_SOLAR","CHUVA","NEVE","BICICLETAS_ALUGADAS")])
dados_cor

#Gráficos
#install.packages("corrplot")
require(corrplot)

corrplot(dados_cor,tl.cex = 0.6)
corrplot(dados_cor,method="color",tl.cex = 0.5)

corrplot(dados_cor,type = "upper",tl.cex = 0.6)
corrplot(dados_cor,method="color",type = "upper",tl.cex = 0.6)

## ----------------------------------------------------------------------
# Etapa 4: Ajustando os modelos
#Ajustando o primeiro modelo
modelo1 <- lm(BICICLETAS_ALUGADAS ~ ., data = base_treino)
summary(modelo1)

#Retirando a variável VISIBILIDADE
modelo2 <- lm(BICICLETAS_ALUGADAS ~ DIA_SEMANA + 
                HORA + 
                TEMPERATURA + 
                UMIDADE + 
                VELOCIDADE_VENTO + 
                RADIACAO_SOLAR + 
                CHUVA + 
                NEVE + 
                ESTACAO + 
                FERIADO + 
                FUNCIONAMENTO, data = base_treino)
summary(modelo2)

# ------------------------------------
#Retirando NEVE
modelo3 <- lm(BICICLETAS_ALUGADAS ~ DIA_SEMANA + 
                HORA + 
                TEMPERATURA + 
                UMIDADE + 
                VELOCIDADE_VENTO + 
                RADIACAO_SOLAR + 
                CHUVA + 
                ESTACAO + 
                FERIADO + 
                FUNCIONAMENTO, data = base_treino)
summary(modelo3)

# ------------------------------------
#Retirando VELOCIDADE_VENTO
modelo4 <- lm(BICICLETAS_ALUGADAS ~ DIA_SEMANA + 
                HORA + 
                TEMPERATURA + 
                UMIDADE + 
                RADIACAO_SOLAR + 
                CHUVA + 
                ESTACAO + 
                FERIADO + 
                FUNCIONAMENTO, data = base_treino)
summary(modelo4)

# ------------------------------------
# Adicionando um indicador para ESTACAO
tapply(base_treino$BICICLETAS_ALUGADAS,base_treino$ESTACAO,summary)

base_treino$ESTACAO_CAT <- ifelse(base_treino$ESTACAO == 'inverno', 1, 0)

#Trocando ESTACAO por ESTACAO_CAT
modelo5 <- lm(BICICLETAS_ALUGADAS ~ DIA_SEMANA + 
                HORA + 
                TEMPERATURA + 
                UMIDADE + 
                RADIACAO_SOLAR + 
                CHUVA + 
                ESTACAO_CAT + 
                FERIADO + 
                FUNCIONAMENTO, data = base_treino)
summary(modelo5)

# ------------------------------------
### PERMANECEMOS COM O MODELO 4
modelo4 <- lm(BICICLETAS_ALUGADAS ~ DIA_SEMANA + 
                HORA + 
                TEMPERATURA + 
                UMIDADE + 
                RADIACAO_SOLAR + 
                CHUVA + 
                ESTACAO + 
                FERIADO + 
                FUNCIONAMENTO, data = base_treino)
summary(modelo4)

#Vamos verificar quem tem a mior correlação e tentar "potencializar a variável"
dados_cor<-cor(base_treino[c("HORA","TEMPERATURA","HUMIDADE","RADIACAO_SOLAR","CHUVA","BICICLETAS_ALUGADAS")])
dados_cor

corrplot(dados_cor,method="color",type = "upper",tl.cex = 0.6)

base_treino$TEMPERATURA2 <- base_treino$TEMPERATURA^2

modelo5 <- lm(BICICLETAS_ALUGADAS ~ DIA_SEMANA + 
                HORA + 
                TEMPERATURA2 + 
                UMIDADE + 
                RADIACAO_SOLAR + 
                CHUVA + 
                ESTACAO + 
                FERIADO + 
                FUNCIONAMENTO, data = base_treino)
summary(modelo5)

# ------------------------------------
### PERMANECEMOS COM O MODELO 4
modelo4 <- lm(BICICLETAS_ALUGADAS ~ DIA_SEMANA + 
                HORA + 
                TEMPERATURA + 
                UMIDADE + 
                RADIACAO_SOLAR + 
                CHUVA + 
                ESTACAO + 
                FERIADO + 
                FUNCIONAMENTO, data = base_treino)
summary(modelo4)


base_treino$CHUVA2 <- base_treino$CHUVA^2

modelo5 <- lm(BICICLETAS_ALUGADAS ~ DIA_SEMANA + 
                HORA + 
                TEMPERATURA + 
                UMIDADE + 
                RADIACAO_SOLAR + 
                CHUVA2 + 
                ESTACAO + 
                FERIADO + 
                FUNCIONAMENTO, data = base_treino)
summary(modelo5)

# ------------------------------------
### PERMANECEMOS COM O MODELO 4
modelo4 <- lm(BICICLETAS_ALUGADAS ~ DIA_SEMANA + 
                HORA + 
                TEMPERATURA + 
                HUMIDADE + 
                RADIACAO_SOLAR + 
                CHUVA + 
                ESTACAO + 
                FERIADO + 
                DIA_FUNCIONAMENTO, data = base_treino)
summary(modelo3)


#Prevendo a quantidade de bicicletas alugadas
previsao_treino <- predict(modelo4)
View(previsao_treino)

previsao_real_treino <- cbind(previsao_treino, base_treino$BICICLETAS_ALUGADAS)
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
previsao_teste <- predict(modelo4, base_teste)
previsao_teste <- as.data.frame(previsao_teste)
View(previsao_teste)

# Visualizando valores de preditos e reais da base de teste
previsao_real_teste <- cbind(previsao_teste, base_teste$BICICLETAS_ALUGADAS)
colnames(previsao_real_teste) <- c('Previsto','Real')
previsao_real_teste <- as.data.frame(previsao_real_teste)
View(previsao_real_teste)

#Calculando o erro médio
#MSE
mse_teste <- mean((previsao_real_teste$Previsto - previsao_real_teste$Real)^2)

#RMSE
rmse_teste <- mse_teste^0.5
rmse_teste

