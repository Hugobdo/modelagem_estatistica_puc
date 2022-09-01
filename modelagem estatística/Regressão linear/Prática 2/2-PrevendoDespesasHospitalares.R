
# Prevendo Despesas Hospitalares

# ------------------------------------------
# Etapa 1 - Verificando  os dados 
despesas <- read.csv("despesas.csv")
View(despesas)

# ------------------------------------------
# Etapa 2 - Separando em base de treino e teste

# install.packages("caret", dependencies = TRUE)
library(caret)

set.seed(50)
indice_treino = createDataPartition(y=despesas$gastos, p=0.7, list=FALSE)
base_treino = despesas[indice_treino, ]
base_teste = despesas[-indice_treino, ]
View(base_treino)
View(base_teste)

# ------------------------------------------
# Etapa 3: Explorando e Preparando os Dados
# Visualizando as variáveis
str(base_treino)

# Medias de tendência Central da variável gastos
boxplot(base_treino$gastos)
abline(h=mean(base_treino$gastos))

# Construindo um histograma
hist(base_treino$gastos, main = 'Histograma', xlab = 'Gastos')

# Explorando relacionamento entre as variáveis: Matriz de correlação
cor(base_treino[c("gastos","idade", "imc", "filhos")])
require(corrplot)
corrplot(cor(base_treino[c("gastos","idade", "imc")]),method="color",tl.cex = 0.6)

# Scatterplot Matrix
#install.packages("psych")
library(psych)

# Este gráfico fornece mais informações sobre o relacionamento entre as variáveis
pairs.panels(base_treino[c("idade", "imc", "filhos", "gastos")])

# ------------------------------------------
# Etapa 3: Treinando o Modelo (usando os dados de treino)

# MODELO 1: Todas as variáveis
modelo1 <- lm(gastos ~ idade + 
                sexo + 
                fumante + 
                filhos + 
                imc + 
                regiao, data = base_treino)
summary(modelo1)

# Similar ao item anterior
modelo1 <- lm(gastos ~ ., data = base_treino)
summary(modelo1)

#MODELO2: Retirando a variável sexo
modelo2 <- lm(gastos ~ idade + 
                fumante + 
                filhos + 
                imc + 
                regiao, data = base_treino)
summary(modelo2)

#MODELO3: Retirando a variável região
modelo3 <- lm(gastos ~ idade + fumante + filhos + imc, data = base_treino)
summary(modelo3)

#MODELO4: Retirando a variável filhos(para verificar se R2 aumenta)
modelo4 <- lm(gastos ~ idade + fumante + imc, data = base_treino)
summary(modelo4)

# Não aumentou, ficaremos com a variável filhos

# Prevendo despesas médicas 
# Aqui verificamos os gastos previstos pelo modelo
previsao_treino <- predict(modelo3)
View(previsao_treino)

previsao_real_treino <- cbind(previsao_treino, base_treino$gastos)
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
previsao_teste <- predict(modelo3, base_teste)
previsao_teste <- as.data.frame(previsao_teste)
View(previsao_teste)

# Visualizando valores de preditos e reais da base de teste
previsao_real_teste <- cbind(previsao_teste, base_teste$gastos)
colnames(previsao_real_teste) <- c('Previsto','Real')
previsao_real_teste <- as.data.frame(previsao_real_teste)
View(previsao_real_teste)

#Calculando o erro médio
#MSE
mse_teste <- mean((previsao_real_teste$Previsto - previsao_real_teste$Real)^2)

#RMSE
rmse_teste <- mse_teste^0.5
rmse_teste

# ------------------------------------------
# Etapa 5: Otimizando a Performance do Modelo
summary(modelo3)

# Adicionando um indicador para IMC >= 30
base_treino$imc30 <- ifelse(base_treino$imc >= 30, 1, 0)

#MODELO5: Inserindo a variável imc categorizada
modelo4 <- lm(gastos ~ idade + fumante + filhos + imc30, data = base_treino)
summary(modelo4)

previsao_treino_otimizacao <- predict(modelo4)
View(previsao_treino_otimizacao)

previsao_real_treino_otimi <- cbind(previsao_treino_otimizacao, base_treino$gastos)
colnames(previsao_real_treino_otimi) <- c('Previsto','Real')
previsao_real_treino_otimi <- as.data.frame(previsao_real_treino_otimi)
View(previsao_real_treino_otimi)

#MSE
mse_treino_otimizacao <- mean((previsao_real_treino_otimi$Previsto - previsao_real_treino_otimi$Real)^2)

#RMSE
rmse_treino_otimizacao <- mse_treino_otimizacao^0.5
rmse_treino_otimizacao

# ------------------------------------------
# Etapa 6: Testando o Modelo (usando os dados de teste)
# Adicionando um indicador para IMC >= 30
base_teste$imc30 <- ifelse(base_teste$imc >= 30, 1, 0)

# Prevendo os gastos com Dados de teste
previsao_teste_otimizacao <- predict(modelo4, base_teste)
previsao_teste_otimizacao <- as.data.frame(previsao_teste_otimizacao)

# Visualizando valores de preditos e reais da base de teste
resultados_teste_otimizacao <- cbind(previsao_teste_otimizacao, base_teste$gastos)
colnames(resultados_teste_otimizacao) <- c('Previsto','Real')
resultados_teste_otimizacao <- as.data.frame(resultados_teste_otimizacao)

#Calculando o erro médio
#MSE
mse_teste_otimizacao <- mean((resultados_teste_otimizacao$Previsto - resultados_teste_otimizacao$Real)^2)

#RMSE
rmse_teste_otimizacao <- mse_teste_otimizacao^0.5
rmse_teste_otimizacao

