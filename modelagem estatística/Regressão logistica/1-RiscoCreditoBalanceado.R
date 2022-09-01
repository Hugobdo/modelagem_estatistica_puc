#Previsão de Risco de Crédito

set.seed(50)
# ------------------------------------------------
# Etapa 1 - Verificando os dados

# Instalando os pacotes
# Permite criar modelo de MAchine Leaning
#install.packages("caret")
#Curva para avaliação do nosso modelo
#install.packages("ROCR")
#install.packages("e1071")

# Carregando os pacotes
library(caret)
library(ROCR) 
library(e1071) 

# Carregando o dataset um um dataframe
credito_dataset <- read.csv("RiscoCredito.csv", header = TRUE, sep = ";")
str(credito_dataset)
View(credito_dataset)


# ------------------------------------------------
# Etapa 2 - Pré-processamento

#Tratando as variáveis categóricas
# Variáveis do tipo fator

for (i in c('classificacao_credito', 'saldo_conta', 'status_pag_cred_ant',
             'finalidade_credito', 'poupanca', 'tempo_emprego', 'taxa_prestacao',
             'estado_civil', 'fiador', 'tempo_residencia', 'ativos',
             'outros_creditos', 'tipo_residencia', 'banco_credito', 'profissao', 
             'dependentes', 'telefone_celular', 'trabalho_estrangeiro')){
  credito_dataset[,i] <- as.factor(credito_dataset[,i])
}

str(credito_dataset)

 --------------------------------------
#Etapa 3 - Preparando os dados de treino e de teste
indice <- createDataPartition(y=credito_dataset$classificacao_credito, p=0.7, list=FALSE)
dados_treino <- credito_dataset[indice,]
dados_teste <- credito_dataset[-indice,]

# --------------------------------------
## Etapa 4 - Balanceando a base de dados

table(dados_treino$classificacao_credito)
prop.table(table(dados_treino$classificacao_credito))
barplot(prop.table(table(dados_treino$classificacao_credito)))


install.packages("tidymodels")
require(tidymodels)
dados_treino_balanceado <-
  recipe(classificacao_credito ~ ., data = dados_treino) %>%
  themis::step_upsample(classificacao_credito) %>%
  prep() %>%
  juice
prop.table(table(dados_treino_balanceado$classificacao_credito))

# ------------------------------------------------
# Etapa 5 - Correlação entre variáveis numéricas

correlacao <- cor(dados_treino_balanceado[c("duracao_credito_meses","valor_credito", "idade")])
correlacao
require(corrplot)
corrplot(correlacao,method="color",tl.cex = 0.8)

# --------------------------------------
#Etapa 6 - Construindo o modelo de regressão logística
modelo1 <- glm(classificacao_credito ~ ., 
               data = dados_treino_balanceado, family = "binomial"(link = "logit"))
summary(modelo1)

# -------------------------------
# Retirando a variável dependentes
modelo2 <- glm(classificacao_credito ~ . - dependentes, 
               data = dados_treino_balanceado, family = "binomial"(link = "logit"))
summary(modelo2)

# Comparando modelo menor com o maior
anova(modelo2, modelo1, test="Chisq") 
# se valor p > niv.sig., as variáveis omitidas não são significativas 

# -------------------------------
# Retirando a variável profissao
modelo3 <- glm(classificacao_credito ~ . - dependentes -profissao, 
               data = dados_treino_balanceado, family = "binomial"(link = "logit"))
summary(modelo3)

# Comparando modelo menor com o maior
anova(modelo3, modelo2, test="Chisq") 
# se valor p > niv.sig., as variáveis omitidas não são significativas 

# -------------------------------
# Retirando a variável tempo_residencia
modelo4 <- glm(classificacao_credito ~ . - dependentes -profissao -tempo_residencia, 
               data = dados_treino_balanceado, family = "binomial"(link = "logit"))
summary(modelo4)

# Comparando modelo menor com o maior
anova(modelo4, modelo3, test="Chisq") 
# se valor p > niv.sig., as variáveis omitidas não são significativas 

# -------------------------------
# Retirando a variável banco_credito
modelo5 <- glm(classificacao_credito ~ . - dependentes -profissao -tempo_residencia
               -banco_credito , 
               data = dados_treino_balanceado, family = "binomial"(link = "logit"))
summary(modelo5)

# Comparando modelo menor com o maior
anova(modelo5, modelo4, test="Chisq") 
# se valor p > niv.sig., as variáveis omitidas não são significativas 

# -------------------------------
# Retirando a variável idade
modelo6 <- glm(classificacao_credito ~ . - dependentes -profissao -tempo_residencia
               -banco_credito -idade, 
               data = dados_treino_balanceado, family = "binomial"(link = "logit"))
summary(modelo6)

# Comparando modelo menor com o maior
anova(modelo6, modelo5, test="Chisq") 
# se valor p > niv.sig., as variáveis omitidas não são significativas 

# -------------------------------
# Retirando a variável outros_creditos
modelo7 <- glm(classificacao_credito ~ . - dependentes -profissao -tempo_residencia
               -banco_credito -idade -outros_creditos, 
               data = dados_treino_balanceado, family = "binomial"(link = "logit"))
summary(modelo7)

# Comparando modelo menor com o maior
anova(modelo7, modelo6, test="Chisq") 
# se valor p > niv.sig., as variáveis omitidas não são significativas 

# -------------------------------
# Retirando a variável fiador
modelo8 <- glm(classificacao_credito ~ . - dependentes -profissao -tempo_residencia
               -banco_credito -idade -outros_creditos -fiador, 
               data = dados_treino_balanceado, family = "binomial"(link = "logit"))
summary(modelo8)

# Comparando modelo menor com o maior
anova(modelo8, modelo7, test="Chisq") 
# se valor p > niv.sig., as variáveis omitidas não são significativas 

# Multicolinearidade
library(car)
vif(modelo8) # valores abaixo de 5 - OK

## ---------------------------------------
##Etapa 6- FAzendo previsões do modelo de treino

#Probabilidade de crédito para cada pessoa
previsoes_treino <- predict(modelo8, dados_treino_balanceado,type = "response")

resultados_treino <- data.frame(previsoes_treino, dados_treino_balanceado$classificacao_credito)
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
matriz_treino = confusionMatrix(resultados_treino$previsto_cat, dados_treino_balanceado$classificacao_credito, positive = "1")
matriz_treino

#Curva ROC
library(ROSE)
roc.curve(dados_treino_balanceado$classificacao_credito, previsoes_treino, plotit = T, col = "red")


resultados_treino$Qtde = 1
require(ggplot2)
ggplot(resultados_treino, aes(fill = real_treino,
                  y = Qtde, x = previsto_faixa))+
  geom_bar(position = "fill", stat = "identity")

## ---------------------------------------
##Etapa 8- FAzendo previsões do modelo de teste

previsoes_teste <- predict(modelo8, dados_teste,type = "response")

resultados_teste <- data.frame(previsoes_teste, dados_teste$classificacao_credito)
resultados_teste$previsao_cat <- as.factor(ifelse(previsoes_teste > 0.5,1,0))

colnames(resultados_teste) <- c('previsto_prob','real_treino','previsto_cat')

matriz_teste = confusionMatrix(resultados_teste$previsto_cat, dados_teste$classificacao_credito, positive = "1")
matriz_teste
