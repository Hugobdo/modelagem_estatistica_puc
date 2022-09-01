## Prevendo a Taxa de ocupação

## Carregando os dados
library(readxl)
dados_Housing <- read_excel("Dados_Housing.xlsx")

# ------------------------------------------
# Etapa 1 - Verificando  os dados 
View(dados_Housing)

# ------------------------------------------
# Etapa 2 - Separando em base de treino e teste

#install.packages("caret", dependencies = TRUE)
library(caret)

set.seed(40)

indice_treino = createDataPartition(y=dados_Housing$TX_OCUPACAO, p=0.7, list=FALSE)
base_treino = dados_Housing[indice_treino, ]
base_teste = dados_Housing[-indice_treino, ]
View(base_treino)
View(base_teste)


# ------------------------------------------
# Etapa 3: Explorando e Preparando os Dados

# Calculando a correlação(para verificar se existe correlação com a variável resposta)
#Valores
str(base_treino)
cor(base_treino)

#Gráficos
#install.packages("corrplot")
require(corrplot)

corrplot(cor(base_treino),tl.cex = 0.6)
corrplot(cor(base_treino),method="color",tl.cex = 0.5)
#corrplot(cor(base_treino),method="ellipse")
#corrplot(cor(base_treino),method="shade")
#corrplot(cor(base_treino),method="number")

corrplot(cor(base_treino),type = "upper",tl.cex = 0.6)
corrplot(cor(base_treino),method="color",type = "upper",tl.cex = 0.6)

#------------------------------------
# Como fizemos a correlação e tivemos as seguintes variáveis altamente correlacionadas:
# dist_boston, poluicao, industria e idade_casa. Uma outra forma seria ajustar o modelo para verificar quais não são significativas
# faço uma análise do problema de negócio, por isso achei melhor ajustar o modelo
modelo <- lm(TX_OCUPACAO ~ ., data = base_treino)
summary(modelo)

# Portando idade_casa e industria não interferem na taxa de ocupação
base_treino$INDUSTRIA <- NULL
base_treino$IDADE_CASA <- NULL

#------------------------------------
# Calculando novamente a correlação
cor(base_treino)
corrplot(cor(base_treino),method="color",tl.cex = 0.5,type = "upper")

modelo2 = lm(TX_OCUPACAO ~ .-IDADE_CASA -POLUICAO, data = base_treino)
summary(modelo2)

#Temos que DIST_BOSTON e POLUICAO são altamente correlacionadas
# o que faz todo sentido, quanto mais próximo ao centro da cidade, maior a poluição
# Vamos tirar DIST_BOSTON que está menos correlacionada com a variável resposta
base_treino$DIST_BOSTON <- NULL

#------------------------------------
# Calculando novamente a correlação
cor(base_treino)
corrplot(cor(base_treino),method="color",tl.cex = 0.5,type = "upper")

# ACESSO_RODOVIA está correlacionado com IMPOSTO
# Sai acesso a rodrovia por possuir menor correlação com a variavel resposta
base_treino$ACESSO_RODOVIA <- NULL


#------------------------------------
# Calculando novamente a correlação
cor(base_treino)
corrplot(cor(base_treino),method="color",tl.cex = 0.5, type = "upper")

## ----------------------------------------------------------------------
# Etapa 4: Ajustando os modelos
#Ajustando o primeiro modelo
modelo1 <- lm(TX_OCUPACAO ~ ., data = base_treino)
summary(modelo1)

# Retirando  IMPOSTO
modelo2 <- lm(TX_OCUPACAO ~ TX_CRIME + 
                PROP_TERRENO + 
                LIMITA_RIO + 
                POLUICAO +
                MEDIA_QUARTOS +
                TX_ESCOLARIDADE + 
                PROP_NEGROS + 
                BAIXA_RENDA, data = base_treino)
summary(modelo2)


# Retirando PROP_TERRENO
modelo3 <- lm(TX_OCUPACAO ~ TX_CRIME + 
                LIMITA_RIO + 
                POLUICAO +
                MEDIA_QUARTOS +
                TX_ESCOLARIDADE + 
                PROP_NEGROS + 
                BAIXA_RENDA, data = base_treino)
summary(modelo3)

# Retirando TX_CRIME
modelo4 <- lm(TX_OCUPACAO ~ LIMITA_RIO + 
                POLUICAO +
                MEDIA_QUARTOS +
                TX_ESCOLARIDADE + 
                PROP_NEGROS + 
                BAIXA_RENDA, data = base_treino)
summary(modelo4)

#Prevendo a taxa de ocupação
previsao_treino <- predict(modelo4)
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

# ------------------------------------------
# Etapa 4: Testando o Modelo (usando os dados de teste)

# Prevendo os gastos com Dados de teste
previsao_teste <- predict(modelo4, base_teste)
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

