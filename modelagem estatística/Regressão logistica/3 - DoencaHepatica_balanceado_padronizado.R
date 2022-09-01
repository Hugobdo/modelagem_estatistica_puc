# Prevendo doença hepática

set.seed(50)

# ------------------------------------------
# Etapa 1 - Verificando  os dados 
library(readxl)
doenca_hepatica <- read_excel("doenca_hepatica.xlsx")
View(doenca_hepatica)

# ------------------------------------------
# Etapa 2 - Realizando análise descritiva
str(doenca_hepatica)

doenca_hepatica$DOENCA_HEPATICA = as.factor(doenca_hepatica$DOENCA_HEPATICA)

table(doenca_hepatica$SEXO)
prop.table(table(doenca_hepatica$SEXO))
#observamos discrepância entre qunatidade de pessoas do sexo masculino e feminino.
#Não faz sentido balancear porque não é a variávle que quero prever

#Dados duplicados
nrow(doenca_hepatica)
sum(duplicated(doenca_hepatica))
require(dplyr)
doenca_hepatica = distinct(doenca_hepatica)
nrow(doenca_hepatica)

#Verificando se tem algum dado ausente
any(is.na(doenca_hepatica))
# percentual de dados faltantes por variável
NAS = round(colSums(is.na(doenca_hepatica)))
NAS[NAS>0]
NAS_PERC = round(colSums(is.na(doenca_hepatica))*100/nrow(doenca_hepatica),2)
NAS_PERC[NAS_PERC>0]

#Podemos excluir as linhas
nrow(doenca_hepatica)
doenca_hepatica = na.omit(doenca_hepatica)
nrow(doenca_hepatica)

#Podemos substituir pela média
#doenca_hepatica$ALBUMINA_GLOBULINA[is.na(doenca_hepatica$ALBUMINA_GLOBULINA)] = mean(doenca_hepatica$ALBUMINA_GLOBULINA,na.rm = TRUE)

# ------------------------------------------
# Etapa 3 - Calculando a correlação
str(doenca_hepatica)
# Explorando relacionamento entre as variáveis: Matriz de correlação
correlacao = cor(doenca_hepatica[c("IDADE","BILIRURBINA_TOTAL","BILIRURBINA_DIRETA","FOSFOTASE_ALCALINA","ALANINA","ASPARTATE",
                      "PROTEINAS_TOTAIS","ALBUMINA","ALBUMINA_GLOBULINA")])
require(corrplot)
corrplot(correlacao,method="color",tl.cex = 0.6)

doenca_hepatica$BILIRURBINA_DIRETA = NULL

# ------------------------------------------
# Etapa 4 - Separando em base de treino e teste

#install.packages("caret", dependencies = TRUE)
library(caret)

indice_treino = createDataPartition(y=doenca_hepatica$DOENCA_HEPATICA, p=0.7, list=FALSE)
base_treino = doenca_hepatica[indice_treino, ]
base_teste = doenca_hepatica[-indice_treino, ]

# ------------------------------------------
## Etapa 5 - Balanceando a base de dados

table(base_treino$DOENCA_HEPATICA)
prop.table(table(base_treino$DOENCA_HEPATICA))
barplot(prop.table(table(base_treino$DOENCA_HEPATICA)))

#install.packages("tidymodels")
require(tidymodels)
dados_treino_balanceado <-
  recipe(DOENCA_HEPATICA ~ ., data = base_treino) %>%
  themis::step_upsample(DOENCA_HEPATICA) %>%
  prep() %>%
  juice
prop.table(table(dados_treino_balanceado$DOENCA_HEPATICA))

# ------------------------------------------
## Etapa 6 - Padronizando a base de dados
#Alguns algoritmos necessitam que os dados estejam na mesma escala.
#Tem que checar as premissas dos algoritmos

library(caret)
padronizacao = preProcess(dados_treino_balanceado, method = c("center","scale"))
# O comando acima cria um modelo de padronização. Para ter efeito ele deve ser aplicado nos dados com o
# comando predict().
dados_treino_balanceado_padronizado = predict(padronizacao,dados_treino_balanceado)

# Ao aplicar um algoritmo no conjunto de teste, só podemos usar os parâmetros que estimamos no conjunto 
# de treino. Ou seja, temos que usar a média e o desvio padrão dos dados de treino.
dados_teste_padronizado = predict(padronizacao,base_teste)

# --------------------------------------
#Etapa 7 - Construindo o modelo de regressão logística
modelo1 <- glm(DOENCA_HEPATICA ~ ., 
               data = dados_treino_balanceado_padronizado, family = "binomial"(link = "logit"))
summary(modelo1)

# -------------------------------
# Retirando a variável SEXO
modelo2 <- glm(DOENCA_HEPATICA ~ . - SEXO, 
               data = dados_treino_balanceado_padronizado, family = "binomial"(link = "logit"))
summary(modelo2)

# Comparando modelo menor com o maior
anova(modelo2, modelo1, test="Chisq") 
# se valor p > niv.sig., as variáveis omitidas não são significativas 

# -------------------------------
# Retirando a variável FOSFOTASE_ALCALINA
modelo3 <- glm(DOENCA_HEPATICA ~ . -SEXO -FOSFOTASE_ALCALINA, 
               data = dados_treino_balanceado_padronizado, family = "binomial"(link = "logit"))
summary(modelo3)

# Comparando modelo menor com o maior
anova(modelo3, modelo2, test="Chisq") 
# se valor p > niv.sig., as variáveis omitidas não são significativas 

# -------------------------------
# Retirando a variável FOSFOTASE_ALCALINA
modelo4 <- glm(DOENCA_HEPATICA ~ . -SEXO -FOSFOTASE_ALCALINA -ALANINA, 
               data = dados_treino_balanceado_padronizado, family = "binomial"(link = "logit"))
summary(modelo4)

# Comparando modelo menor com o maior
anova(modelo4, modelo3, test="Chisq") 
# se valor p > niv.sig., as variáveis omitidas não são significativas

#Comparando os dois AICs
AIC(modelo1)
AIC(modelo4)

# Multicolinearidade
library(car)
vif(modelo5) # valores abaixo de 5 - OK

# -------------------------------
correlacao = cor(doenca_hepatica[c("IDADE","BILIRURBINA_TOTAL","ASPARTATE",
                                   "PROTEINAS_TOTAIS","ALBUMINA","ALBUMINA_GLOBULINA")])
require(corrplot)
corrplot(correlacao,method="color",tl.cex = 0.6)

# Retirando a variável ALBUMINA
modelo5 <- glm(DOENCA_HEPATICA ~ . -SEXO -FOSFOTASE_ALCALINA -ALANINA -ALBUMINA, 
               data = dados_treino_balanceado_padronizado, family = "binomial"(link = "logit"))
summary(modelo5)

# Comparando modelo menor com o maior
anova(modelo5, modelo4, test="Chisq") 


# Retirando a variável ALBUMINA
modelo6 <- glm(DOENCA_HEPATICA ~ . -SEXO -FOSFOTASE_ALCALINA -ALANINA -ALBUMINA -ALBUMINA_GLOBULINA, 
               data = dados_treino_balanceado_padronizado, family = "binomial"(link = "logit"))
summary(modelo6)

# Comparando modelo menor com o maior
anova(modelo6, modelo5, test="Chisq") 

# Retirando a variável ALBUMINA
modelo7 <- glm(DOENCA_HEPATICA ~ . -SEXO -FOSFOTASE_ALCALINA -ALANINA -ALBUMINA -ALBUMINA_GLOBULINA -PROTEINAS_TOTAIS, 
               data = dados_treino_balanceado_padronizado, family = "binomial"(link = "logit"))
summary(modelo7)

# Comparando modelo menor com o maior
anova(modelo7, modelo6, test="Chisq") 

# Multicolinearidade
library(car)
vif(modelo7) # valores abaixo de 5 - OK



## ---------------------------------------
##Etapa 8 - FAzendo previsões do modelo de treino

#Probabilidade da pessoa desenvolver ou não doença hepática
previsoes_treino <- predict(modelo7, dados_treino_balanceado_padronizado,type = "response")

resultados_treino <- data.frame(previsoes_treino, dados_treino_balanceado_padronizado$DOENCA_HEPATICA)
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
##Etapa 9- Desempenho do modelo de treino


# Matriz de confusão e medidas
library(caret)
matriz_confusao_treino = confusionMatrix(resultados_treino$previsto_cat, dados_treino_balanceado_padronizado$DOENCA_HEPATICA, positive = "1")
matriz_confusao_treino

#Curva ROC
library(ROSE)
roc.curve(dados_treino_balanceado_padronizado$DOENCA_HEPATICA, previsoes_treino, plotit = T, col = "red")


resultados_treino$Qtde = 1
require(ggplot2)
ggplot(resultados_treino, aes(fill = real_treino,
                              y = Qtde, x = previsto_faixa))+
  geom_bar(position = "fill", stat = "identity")

## ---------------------------------------
##Etapa 8- FAzendo previsões do modelo de teste

previsoes_teste <- predict(modelo7, dados_teste_padronizado,type = "response")

resultados_teste <- data.frame(previsoes_teste, dados_teste_padronizado$DOENCA_HEPATICA)
resultados_teste$previsao_cat <- as.factor(ifelse(previsoes_teste > 0.5,1,0))

colnames(resultados_teste) <- c('previsto_prob','real_treino','previsto_cat')

matriz_confusao_teste = confusionMatrix(resultados_teste$previsto_cat, dados_teste_padronizado$DOENCA_HEPATICA, positive = "1")
matriz_confusao_teste

matriz_confusao_treino
