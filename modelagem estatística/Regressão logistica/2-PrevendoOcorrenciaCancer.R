# Prevendo a Ocorrência de Câncer

# ------------------------------------------------
# Etapa 1 - Verificando os dados
# Definindo o diretório de trabalho


dados <- read.csv("ocorrencia_cancer.csv", header = TRUE, sep = ";",dec = ".")
str(dados)
View(dados)

# ------------------------------------------------
# Etapa 2 - Pré-processamento
# Categorizando as variável resposta

dados$DIAGNOSTICO_CAT <- as.factor(ifelse(dados$DIAGNOSTICO == 'M',1,0))
dados$DIAGNOSTICO <- NULL

# ------------------------------------------------
# Etapa 3 - Correlação entre variáveis numéricas

correlacao <- cor(subset(dados, select = -c(DIAGNOSTICO_CAT)))
correlacao
require(corrplot)
corrplot(correlacao,method="color",tl.cex = 0.6)

dados$RAIO_MEDIO <- NULL
correlacao <- cor(subset(dados, select = -c(DIAGNOSTICO_CAT)))
corrplot(correlacao,method="color",tl.cex = 0.6)


# --------------------------------------
#Etapa 4 - Preparando os dados de treino e de teste
indice <- createDataPartition(y=dados$DIAGNOSTICO_CAT, p=0.7, list=FALSE)
dados_treino <- dados[indice,]
dados_teste <- dados[-indice,]

table(dados_treino$DIAGNOSTICO_CAT)
prop.table(table(dados_treino$DIAGNOSTICO_CAT))

require(tidymodels)
dados_treino_balanceado <-
  recipe(DIAGNOSTICO_CAT ~ ., data = dados_treino) %>%
  themis::step_upsample(DIAGNOSTICO_CAT) %>%
  prep() %>%
  juice
prop.table(table(dados_treino_balanceado$DIAGNOSTICO_CAT))


# --------------------------------------
#Etapa 5 - Construindo o modelo de regressão logística
modelo1 <- glm(DIAGNOSTICO_CAT ~ ., 
               data = dados_treino_balanceado, family = "binomial"(link = "logit"))
summary(modelo1)

# -------------------------------
# Retirando a variável DIMENSAO_MEDIA     
modelo2 <- glm(DIAGNOSTICO_CAT ~ . -DIMENSAO_MEDIA, 
               data = dados_treino_balanceado, family = "binomial"(link = "logit"))
summary(modelo2)

anova(modelo2, modelo1, test="Chisq") 


# -------------------------------
# Retirando a variável CONCAVIDADE_MEDIA     
modelo3 <- glm(DIAGNOSTICO_CAT ~ . -DIMENSAO_MEDIA -CONCAVIDADE_MEDIA, 
               data = dados_treino_balanceado, family = "binomial"(link = "logit"))
summary(modelo3)

anova(modelo3, modelo2, test="Chisq") 

# -------------------------------
# Retirando a variável COMPACIDADE_MEDIA     
modelo4 <- glm(DIAGNOSTICO_CAT ~ . -DIMENSAO_MEDIA -CONCAVIDADE_MEDIA -COMPACIDADE_MEDIA, 
               data = dados_treino_balanceado, family = "binomial"(link = "logit"))
summary(modelo4)

anova(modelo4, modelo3, test="Chisq") 

# -------------------------------
# Retirando a variável SUAVIDADE_MEDIA     
modelo5 <- glm(DIAGNOSTICO_CAT ~ . -DIMENSAO_MEDIA -CONCAVIDADE_MEDIA -COMPACIDADE_MEDIA -SUAVIDADE_MEDIA, 
               data = dados_treino_balanceado, family = "binomial"(link = "logit"))
summary(modelo5)

anova(modelo5, modelo4, test="Chisq") 

# Multicolinearidade
library(car)
vif(modelo5) # valores abaixo de 5 - OK

# -------------------------------
#vAMOS TIRAR PERIMETRO_MEDIO que é menos significativo para o modelo
modelo6 <- glm(DIAGNOSTICO_CAT ~ . -DIMENSAO_MEDIA -CONCAVIDADE_MEDIA -COMPACIDADE_MEDIA -SUAVIDADE_MEDIA
               -PERIMETRO_MEDIO, 
               data = dados_treino_balanceado, family = "binomial"(link = "logit"))
summary(modelo6)

anova(modelo6, modelo5, test="Chisq") 

# Multicolinearidade
library(car)
vif(modelo6)


## ---------------------------------------
##Etapa 6- FAzendo previsões do modelo de treino

#Probabilidade de crédito para cada pessoa
previsoes_treino <- predict(modelo6, dados_treino_balanceado,type = "response")

resultados_treino <- data.frame(previsoes_treino, dados_treino_balanceado$DIAGNOSTICO_CAT)
resultados_treino$previsao_cat <- as.factor(ifelse(previsoes_treino > 0.5,1,0))

library(dplyr)
resultados_treino <- resultados_treino %>%
  mutate(faixa = 
           case_when(previsoes_treino <= 0.10 ~ "0-FX10",
                     (previsoes_treino > 0.10 & previsoes_treino <= 0.20) ~ "1-FX20",
                     (previsoes_treino > 0.20 & previsoes_treino <= 0.30) ~ "2-FX30",
                     (previsoes_treino > 0.30 & previsoes_treino <= 0.40) ~ "3-FX40",
                     (previsoes_treino > 0.40 & previsoes_treino <= 0.50) ~ "4-FX50",
                     (previsoes_treino > 0.50 & previsoes_treino <= 0.60) ~ "5-FX60",
                     (previsoes_treino > 0.60 & previsoes_treino <= 0.70) ~ "6-FX70",
                     (previsoes_treino > 0.70 & previsoes_treino <= 0.80) ~ "7-FX80",
                     (previsoes_treino > 0.80 & previsoes_treino <= 0.90) ~ "8-FX90",
                     (previsoes_treino > 0.00 & previsoes_treino <= 1) ~ "9-FX100"))

colnames(resultados_treino) <- c('previsto_prob','real_treino','previsto_cat',"previsto_faixa")


## ---------------------------------------
##Etapa 7- Desempenho do modelo de treino

# Matriz de confusão e medidas
library(caret)
confusionMatrix(resultados_treino$previsto_cat, dados_treino_balanceado$DIAGNOSTICO_CAT, positive = "1")


#Curva ROC
library(ROSE)
roc.curve(dados_treino_balanceado$DIAGNOSTICO_CAT, previsoes_treino, plotit = T, col = "red")


#Identificação melhor ponto de corte para a melhor combinação entre sensibilidade e especificidade
#library(pROC)
#auc <- roc(dados_treino_balanceado$classificacao_credito, previsoes_treino)
#plot.roc(auc, print.thres = T) # descobrimos o ponto de corte que fornece melhor soma de S e E

# O melhor ponto de corte seria 0.444
#resultados_treino$previsao_cat2 <- as.factor(ifelse(previsoes_treino > 0.444,1,0))

resultados_treino$Qtde = 1
require(ggplot2)
ggplot(resultados_treino, aes(fill = real_treino,
                              y = Qtde, x = previsto_faixa))+
  geom_bar(position = "fill", stat = "identity")


#confusionMatrix(resultados_treino$previsao_cat2, dados_treino_balanceado$classificacao_credito, positive = "1")

## ---------------------------------------
##Etapa 8- FAzendo previsões do modelo de teste

previsoes_teste <- predict(modelo6, dados_teste,type = "response")

resultados_teste <- data.frame(previsoes_teste, dados_teste$DIAGNOSTICO_CAT)
resultados_teste$previsao_cat <- as.factor(ifelse(previsoes_teste > 0.5,1,0))

colnames(resultados_teste) <- c('previsto_prob','real_treino','previsto_cat')

confusionMatrix(resultados_teste$previsto_cat, dados_teste$DIAGNOSTICO_CAT, positive = "1")


## ---------------------------------------
##Etapa 9- Mudando o limiar para 0,7
resultados_treino$previsao_cat2 <- as.factor(ifelse(previsoes_treino > 0.7,1,0))
confusionMatrix(resultados_treino$previsao_cat2, dados_treino_balanceado$DIAGNOSTICO_CAT, positive = "1")




