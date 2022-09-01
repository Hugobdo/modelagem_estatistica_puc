#Previsão de Risco de Crédito

# ------------------------------------------------
# Etapa 1 - Verificando os dados
# Definindo o diretório de trabalho


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

# ------------------------------------------------
# Etapa 3 - Correlação entre variáveis numéricas

correlacao <- cor(credito_dataset[c("duracao_credito_meses","valor_credito", "idade")])
correlacao
require(corrplot)
corrplot(correlacao,method="color",tl.cex = 0.8)


# --------------------------------------
#Etapa 4 - Preparando os dados de treino e de teste
indice <- createDataPartition(y=credito_dataset$classificacao_credito, p=0.7, list=FALSE)
dados_treino <- credito_dataset[indice,]
dados_teste <- credito_dataset[-indice,]

table(dados_treino$classificacao_credito)
prop.table(table(dados_treino$classificacao_credito))

# --------------------------------------
#Etapa 5 - Construindo o modelo de regressão logística
modelo1 <- glm(classificacao_credito ~ ., data = dados_treino, family = "binomial"(link = "logit"))
summary(modelo1)

# -------------------------------
# Retirando a variável profissao
modelo2 <- glm(classificacao_credito ~ . - profissao, data = dados_treino, family = "binomial"(link = "logit"))
summary(modelo2)

# Comparando modelo menor com o maior
anova(modelo2, modelo1, test="Chisq") 
# se valor p > niv.sig., as variáveis omitidas não são significativas 

# -------------------------------
# Retirando a variável idade
modelo3 <- glm(classificacao_credito ~ . - profissao -idade, data = dados_treino, family = "binomial"(link = "logit"))
summary(modelo3)

# Comparando modelo menor com o maior
anova(modelo3, modelo2, test="Chisq") 
# se valor p > niv.sig., as variáveis omitidas não são significativas 

# -------------------------------
# Retirando a variável dependentes
modelo4 <- glm(classificacao_credito ~ . - profissao -idade -dependentes, data = dados_treino, family = "binomial"(link = "logit"))
summary(modelo4)

# Comparando modelo menor com o maior
anova(modelo4, modelo3, test="Chisq") 
# se valor p > niv.sig., as variáveis omitidas não são significativas 

# -------------------------------
# Retirando a variável banco_credito
modelo5 <- glm(classificacao_credito ~ . - profissao -idade -dependentes
               - banco_credito, data = dados_treino, family = "binomial"(link = "logit"))
summary(modelo5)

# Comparando modelo menor com o maior
anova(modelo5, modelo4, test="Chisq") 
# se valor p > niv.sig., as variáveis omitidas não são significativas 

# -------------------------------
# Retirando a variável telefone_celular
modelo6 <- glm(classificacao_credito ~ . - profissao -idade -dependentes
               - banco_credito -telefone_celular, data = dados_treino, family = "binomial"(link = "logit"))
summary(modelo6)

# Comparando modelo menor com o maior
anova(modelo6, modelo5, test="Chisq") 
# se valor p > niv.sig., as variáveis omitidas não são significativas 

# -------------------------------
# Retirando a variável ativos
modelo7 <- glm(classificacao_credito ~ . - profissao -idade -dependentes
               - banco_credito -telefone_celular -ativos, data = dados_treino, family = "binomial"(link = "logit"))
summary(modelo7)

# Comparando modelo menor com o maior
anova(modelo7, modelo6, test="Chisq") 
# se valor p > niv.sig., as variáveis omitidas não são significativas 

# -------------------------------
# Retirando a variável outros_creditos
modelo8 <- glm(classificacao_credito ~ . - profissao -idade -dependentes
               - banco_credito -telefone_celular -ativos
               -outros_creditos, data = dados_treino, family = "binomial"(link = "logit"))
summary(modelo8)

# Comparando modelo menor com o maior
anova(modelo8, modelo7, test="Chisq") 
# se valor p > niv.sig., as variáveis omitidas não são significativas 

# -------------------------------
# Retirando a variável trabalho_estrangeiro
modelo9 <- glm(classificacao_credito ~ . - profissao -idade -dependentes
               - banco_credito -telefone_celular -ativos
               -outros_creditos -trabalho_estrangeiro, data = dados_treino , family = "binomial"(link = "logit"))
summary(modelo9)

# Comparando modelo menor com o maior
anova(modelo9, modelo8, test="Chisq") 
# se valor p > niv.sig., as variáveis omitidas não são significativas 

# -------------------------------
# Retirando a variável fiador
modelo10 <- glm(classificacao_credito ~ . - profissao -idade -dependentes
                - banco_credito -telefone_celular -ativos
                -outros_creditos -trabalho_estrangeiro -fiador, data = dados_treino , family = "binomial"(link = "logit"))
summary(modelo10)


# Comparando modelo menor com o maior
anova(modelo10, modelo9, test="Chisq") 
# se valor p > niv.sig., as variáveis omitidas não são significativas 

# -------------------------------
# Retirando a variável tipo_residencia
modelo11 <- glm(classificacao_credito ~ . - profissao -idade -dependentes
                - banco_credito -telefone_celular -ativos
                -outros_creditos -trabalho_estrangeiro -fiador -tipo_residencia, data = dados_treino , family = "binomial"(link = "logit"))
summary(modelo11)


# Comparando modelo menor com o maior
anova(modelo11, modelo10, test="Chisq") 
# se valor p > niv.sig., as variáveis omitidas não são significativas 

# -------------------------------
# Retirando a variável tempo_emprego
modelo12 <- glm(classificacao_credito ~ . - profissao -idade -dependentes
                - banco_credito -telefone_celular -ativos
                -outros_creditos -trabalho_estrangeiro -fiador -tipo_residencia
                - tempo_emprego, data = dados_treino , family = "binomial"(link = "logit"))
summary(modelo12)


# Comparando modelo menor com o maior
anova(modelo12, modelo11, test="Chisq") 
# se valor p > niv.sig., as variáveis omitidas não são significativas 

# -------------------------------
# PERMANECENDO COM A variável tempo_emprego
# RETIRANDO A variável finalidade_credito

summary(modelo11)
modelo12 <- glm(classificacao_credito ~ . - profissao -idade -dependentes
                - banco_credito -telefone_celular -ativos
                -outros_creditos -trabalho_estrangeiro -fiador -tipo_residencia
                - finalidade_credito, data = dados_treino , family = "binomial"(link = "logit"))
summary(modelo12)


# Comparando modelo menor com o maior
anova(modelo12, modelo11, test="Chisq") 
# se valor p > niv.sig., as variáveis omitidas não são significativas 

# -------------------------------
# PERMANECENDO COM A variável finalidade_credito
# RETIRANDO A variável poupanca

summary(modelo11)
modelo12 <-  glm(classificacao_credito ~ . - profissao -idade -dependentes
                 - banco_credito -telefone_celular -ativos
                 -outros_creditos -trabalho_estrangeiro -fiador -tipo_residencia
                 - poupanca, data = dados_treino , family = "binomial"(link = "logit"))
summary(modelo12)


# Comparando modelo menor com o maior
anova(modelo12, modelo11, test="Chisq") 
# se valor p > niv.sig., as variáveis omitidas não são significativas 

# -------------------------------
# PERMANECENDO COM A variável poupanca
# RETIRANDO A variável taxa_prestacao

summary(modelo11)
modelo12 <- glm(classificacao_credito ~ . - profissao -idade -dependentes
                - banco_credito -telefone_celular -ativos
                -outros_creditos -trabalho_estrangeiro -fiador -tipo_residencia
                - taxa_prestacao, data = dados_treino , family = "binomial"(link = "logit"))
summary(modelo12)


# Comparando modelo menor com o maior
anova(modelo12, modelo11, test="Chisq") 

# -------------------------------
# PERMANECENDO COM A variável taxa_prestacao
# RETIRANDO A variável tempo_residencia

summary(modelo11)
modelo12 <- glm(classificacao_credito ~ . - profissao -idade -dependentes
                - banco_credito -telefone_celular -ativos
                -outros_creditos -trabalho_estrangeiro -fiador -tipo_residencia
                - tempo_residencia, data = dados_treino , family = "binomial"(link = "logit"))
summary(modelo12)


# Comparando modelo menor com o maior
anova(modelo12, modelo11, test="Chisq") 


# Multicolinearidade
library(car)
vif(modelo12) # valores abaixo de 5 - OK


## ---------------------------------------
##Etapa 6- FAzendo previsões do modelo de treino

#Probabilidade de crédito para cada pessoa
previsoes_treino <- predict(modelo12, dados_treino,type = "response")

resultados_treino <- data.frame(previsoes_treino, dados_treino$classificacao_credito)
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
matriz_treino = confusionMatrix(resultados_treino$previsto_cat, dados_treino$classificacao_credito, positive = "1")
matriz_treino

#Curva ROC
library(ROSE)
roc.curve(dados_treino$classificacao_credito, previsoes_treino, plotit = T, col = "red")


resultados_treino$Qtde = 1
require(ggplot2)
ggplot(resultados_treino, aes(fill = real_treino,
                  y = Qtde, x = previsto_faixa))+
  geom_bar(position = "fill", stat = "identity")

ggplot(resultados_treino, aes(fill = real_treino,
                              y = Qtde, x = previsto_faixa))+
  geom_bar(stat = "identity")

## ---------------------------------------
##Etapa 5- FAzendo previsões do modelo de teste

previsoes_teste <- predict(modelo12, dados_teste,type = "response")

resultados_teste <- data.frame(previsoes_teste, dados_teste$classificacao_credito)
resultados_teste$previsao_cat <- as.factor(ifelse(previsoes_teste > 0.5,1,0))

colnames(resultados_teste) <- c('previsto_prob','real_treino','previsto_cat')

matriz_teste = confusionMatrix(resultados_teste$previsto_cat, dados_teste$classificacao_credito, positive = "1")
matriz_teste
