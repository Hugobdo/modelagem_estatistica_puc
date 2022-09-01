# Prevendo Saldo

set.seed(50)

# ------------------------------------------
# Etapa 1 - Verificando  os dados 
library(readxl)
emprego <- read_excel("EXERCICIO1_2_Emprego.xlsx")
emprego$saldo_emprego <- ifelse(emprego$saldo_emprego >= "Positivo", 1, 0)
View(emprego)

# ------------------------------------------
# Etapa 2 - Realizando análise descritiva
str(emprego)

emprego$saldo_emprego = as.factor(emprego$saldo_emprego)

table(emprego$saldo_emprego)
prop.table(table(emprego$saldo_emprego))
#observamos discrepância entre qunatidade de saldo negativo e saldo positivo

#Dados duplicados - n foram encontrados dados duplicados
nrow(emprego)
sum(duplicated(emprego))
require(dplyr)
emprego = distinct(emprego)
nrow(emprego)

#Verificando se tem algum dado ausente
any(is.na(emprego))
# percentual de dados faltantes por variável - Não foram encontrados dados faltantes
NAS = round(colSums(is.na(emprego)))
NAS[NAS>0]
NAS_PERC = round(colSums(is.na(emprego))*100/nrow(emprego),2)
NAS_PERC[NAS_PERC>0]

# ------------------------------------------
# Etapa 3 - Calculando a correlação
str(emprego)
# Explorando relacionamento entre as variáveis: Matriz de correlação
correlacao = cor(emprego[c("PIB","inflacao","taxa_juros","taxa_cambio","patentes")], method = 'kendall')
require(corrplot)
corrplot(correlacao,method="color",tl.cex = 0.6)


emprego$PIB = NULL

# ------------------------------------------
# Etapa 4 - Separando em base de treino e teste

#install.packages("caret", dependencies = TRUE)
library(caret)

indice_treino = createDataPartition(y=emprego$saldo_emprego, p=0.7, list=FALSE)
base_treino = emprego[indice_treino, ]
base_teste = emprego[-indice_treino, ]

# ------------------------------------------
## Etapa 5 - Balanceando a base de dados

table(base_treino$saldo_emprego)
prop.table(table(base_treino$saldo_emprego))
barplot(prop.table(table(base_treino$saldo_emprego)))

#install.packages("tidymodels")
require(tidymodels)
dados_treino_balanceado <-
  recipe(saldo_emprego ~ ., data = base_treino) %>%
  themis::step_upsample(saldo_emprego) %>%
  prep() %>%
  juice
prop.table(table(dados_treino_balanceado$saldo_emprego))
barplot(prop.table(table(dados_treino_balanceado$saldo_emprego)))


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
modelo1 <- glm(saldo_emprego ~ ., 
               data = dados_treino_balanceado_padronizado, family = "binomial"(link = "logit"))
summary(modelo1)

# -------------------------------
# Retirando a variável SEXO
modelo2 <- glm(saldo_emprego ~ . - taxa_juros, 
               data = dados_treino_balanceado_padronizado, family = "binomial"(link = "logit"))
summary(modelo2)

# Comparando modelo menor com o maior
anova(modelo2, modelo1, test="Chisq") 
# se valor p > niv.sig., as variáveis omitidas não são significativas 

#Comparando os dois AICs
AIC(modelo1)
AIC(modelo2)

# Multicolinearidade
library(car)
vif(modelo2) # valores abaixo de 5 - OK

## ---------------------------------------
##Etapa 8 - FAzendo previsões do modelo de treino

#Probabilidade da pessoa desenvolver ou não doença hepática
previsoes_treino <- predict(modelo2, dados_treino_balanceado_padronizado,type = "response")

resultados_treino <- data.frame(previsoes_treino, dados_treino_balanceado_padronizado$saldo_emprego)
resultados_treino$previsao_cat <- as.factor(ifelse(previsoes_treino > 0.4,1,0))

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
                     (previsoes_treino > 0.90 & previsoes_treino <= 1) ~ "9-FX100"))

colnames(resultados_treino) <- c('previsto_prob','real_treino','previsto_cat',"previsto_faixa")


## ---------------------------------------
##Etapa 9- Desempenho do modelo de treino


# Matriz de confusão e medidas
library(caret)
matriz_confusao_treino = confusionMatrix(resultados_treino$previsto_cat, dados_treino_balanceado_padronizado$saldo_emprego, positive = "1")
matriz_confusao_treino

#Curva ROC
library(ROSE)
roc.curve(dados_treino_balanceado_padronizado$saldo_emprego, previsoes_treino, plotit = T, col = "red")


resultados_treino$Qtde = 1
require(ggplot2)
ggplot(resultados_treino, aes(fill = real_treino,
                              y = Qtde, x = previsto_faixa))+
  geom_bar(position = "fill", stat = "identity")

## ---------------------------------------
##Etapa 8- Fazendo previsões do modelo de teste

previsoes_teste <- predict(modelo2, dados_teste_padronizado,type = "response")

resultados_teste <- data.frame(previsoes_teste, dados_teste_padronizado$saldo_emprego)
resultados_teste$previsao_cat <- as.factor(ifelse(previsoes_teste > 0.4,1,0))

colnames(resultados_teste) <- c('previsto_prob','real_treino','previsto_cat')

matriz_confusao_teste = confusionMatrix(resultados_teste$previsto_cat, dados_teste_padronizado$saldo_emprego, positive = "1")
matriz_confusao_teste

matriz_confusao_treino

draw_confusion_matrix <- function(cm) {
  
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('CONFUSION MATRIX', cex.main=2)
  
  # create the matrix 
  rect(150, 430, 240, 370, col='#3F97D0')
  text(195, 435, 'Negative', cex=1.2)
  rect(250, 430, 340, 370, col='#F7AD50')
  text(295, 435, 'Positive', cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col='#F7AD50')
  rect(250, 305, 340, 365, col='#3F97D0')
  text(140, 400, 'Negative', cex=1.2, srt=90)
  text(140, 335, 'Positive', cex=1.2, srt=90)
  
  # add in the cm results 
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')
  
  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
  text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  text(30, 85, names(cm$byClass[2]), cex=1.2, font=2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(50, 85, names(cm$byClass[5]), cex=1.2, font=2)
  text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
  text(70, 85, names(cm$byClass[6]), cex=1.2, font=2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cm$byClass[7]), cex=1.2, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)
  
  # add in the accuracy information 
  text(30, 35, names(cm$overall[1]), cex=1.5, font=2)
  text(30, 20, round(as.numeric(cm$overall[1]), 3), cex=1.4)
  text(70, 35, names(cm$overall[2]), cex=1.5, font=2)
  text(70, 20, round(as.numeric(cm$overall[2]), 3), cex=1.4)
}  

draw_confusion_matrix(matriz_confusao_teste)
draw_confusion_matrix(matriz_confusao_treino)

