# Prevendo lucro bruto de uma rede de varejo

# ------------------------------------------
# Etapa 1 - Verificando  os dados
library(readxl)
lucrobruto_rededevarejo <- read_excel("lucrobruto_rededevarejo.xlsx")
View(lucrobruto_rededevarejo)

# ------------------------------------------
# Etapa 2 - Separando em base de treino e teste

#install.packages("caret", dependencies = TRUE)
library(caret)

indice_treino = createDataPartition(y=lucrobruto_rededevarejo$lucro_bruto, p=0.7, list=FALSE)
base_treino = lucrobruto_rededevarejo[indice_treino, ]
base_teste = lucrobruto_rededevarejo[-indice_treino, ]
View(base_treino)
View(base_teste)

# ------------------------------------------
# Etapa 3: Explorando e Preparando os Dados

# Calculando a correlação(para verificar se existe correlação com a variável resposta)
#Valores
str(base_treino)
dados_cor<-cor(base_treino)
dados_cor

#Gráficos
#install.packages("corrplot")
require(corrplot)

corrplot(dados_cor,tl.cex = 0.6)
corrplot(dados_cor,method="color",tl.cex = 0.5)

corrplot(dados_cor,type = "upper",tl.cex = 0.6)
corrplot(dados_cor,method="color",type = "upper",tl.cex = 0.6)


# -----------------------------
# inflacao12meses corrrelacionada com numero_lojas, funcionarios, variedade_produtos, Mês
# retirar inflacao12meses
base_treino$inflacao12meses <- NULL
dados_cor<-cor(base_treino)
dados_cor
corrplot(dados_cor,method="color",type = "upper", tl.cex = 0.6)

# Como a correlação de todas as variáveis estão muito parcidas, vamos ajustar o modelo

## ----------------------------------------------------------------------
# Etapa 4: Ajustando os modelos
#Ajustando o primeiro modelo
modelo1 <- lm(lucro_bruto ~ ., data = base_treino)
summary(modelo1)

#Retirando ano
modelo2 <- lm(lucro_bruto ~ funcionarios + variedade_produtos +  
                mês + semana + investimento_market + numero_lojas + 
                score_spc + seguidores, data = base_treino)
summary(modelo2)

#Retirando variedade_produtos
modelo3 <- lm(lucro_bruto ~ funcionarios +   
                mês + semana + investimento_market + numero_lojas + 
                score_spc + seguidores, data = base_treino)
summary(modelo3)

#Retirando mÊs
modelo4 <- lm(lucro_bruto ~ funcionarios +   
                semana + investimento_market + numero_lojas + 
                score_spc + seguidores, data = base_treino)
summary(modelo4)



base_treino$ano = NULL
base_treino$variedade_produtos  = NULL
base_treino$mês = NULL

dados_cor<-cor(base_treino)
dados_cor

corrplot(dados_cor,method="color",type = "upper",tl.cex = 0.6)

#Retirando FUNCIONÁRIOS
modelo5 <- lm(lucro_bruto ~ semana + investimento_market + numero_lojas + 
                score_spc + seguidores, data = base_treino)
summary(modelo5)

#Prevendo o lucro bruto
previsao_treino <- predict(modelo4)
previsao_real_treino <- cbind(previsao_treino, base_treino$lucro_bruto)
colnames(previsao_real_treino) <- c('Previsto','Real')
previsao_real_treino <- as.data.frame(previsao_real_treino)

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

# Visualizando valores de preditos e reais da base de teste
previsao_real_teste <- cbind(previsao_teste, base_teste$lucro_bruto)
colnames(previsao_real_teste) <- c('Previsto','Real')
previsao_real_teste <- as.data.frame(previsao_real_teste)

#Calculando o erro médio
#MSE
mse_teste <- mean((previsao_real_teste$Previsto - previsao_real_teste$Real)^2)

#RMSE
rmse_teste <- mse_teste^0.5
rmse_teste

# -------------------------------------------------------
# Para o modelo sem a variável funcionários
#Prevendo o lucro bruto
previsao_treino2 <- predict(modelo5)
previsao_real_treino2 <- cbind(previsao_treino2, base_treino$lucro_bruto)
colnames(previsao_real_treino2) <- c('Previsto','Real')
previsao_real_treino2 <- as.data.frame(previsao_real_treino2)

#MSE
mse_treino2 <- mean((previsao_real_treino2$Previsto - previsao_real_treino2$Real)^2)

#RMSE
rmse_treino2 <- mse_treino2^0.5
rmse_treino2

# ------------------------------------------
# Etapa 4: Testando o Modelo (usando os dados de teste)

# Prevendo os gastos com Dados de teste
previsao_teste2 <- predict(modelo5, base_teste)
previsao_teste2 <- as.data.frame(previsao_teste2)

# Visualizando valores de preditos e reais da base de teste
previsao_real_teste2 <- cbind(previsao_teste2, base_teste$lucro_bruto)
colnames(previsao_real_teste2) <- c('Previsto','Real')
previsao_real_teste2 <- as.data.frame(previsao_real_teste2)

#Calculando o erro médio
#MSE
mse_teste2 <- mean((previsao_real_teste2$Previsto - previsao_real_teste2$Real)^2)

#RMSE
rmse_teste2 <- mse_teste2^0.5
rmse_teste2
