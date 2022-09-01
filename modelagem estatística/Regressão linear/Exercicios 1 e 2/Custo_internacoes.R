# Prevendo o consumo de internacoes

# ------------------------------------------
# Etapa 1 - Verificando  os dados 
library(readxl)
internacoes <- read_excel("EXERCICIO2_Custo_Internacoes.xlsx")
View(internacoes)


# ------------------------------------------
# Etapa 2 - Separando em base de treino e teste

#install.packages("caret", dependencies = TRUE)
library(caret)

set.seed(50)
indice_treino = createDataPartition(y=internacoes$despesas_hospitalares, p=0.7, list=FALSE)
base_treino = internacoes[indice_treino, ]
base_teste = internacoes[-indice_treino, ]
View(base_treino)
View(base_teste)

# ------------------------------------------
# Etapa 3: Explorando e Preparando os Dados

# Calculando a correlação(para verificar se existe correlação com a variável resposta)
#Valores
str(base_treino)
dados_cor<-cor(base_treino[c(
  "saturacao",
  "idade",
  "internacoes12meses",
  "consultas30dias",
  "fumante",
  "urgencia",
  "creatinina",
  "pressao_arterial",
  "glicemia",
  "despesas_hospitalares")
  ]
)

#Gráficos
#install.packages("corrplot")
require(corrplot)

corrplot(dados_cor,tl.cex = 0.6)
corrplot(dados_cor,method="color",tl.cex = 0.5)

corrplot(dados_cor,type = "upper",tl.cex = 0.6)
corrplot(dados_cor,method="color",type = "upper",tl.cex = 0.6)
corrplot(dados_cor,method="number",type = "upper",tl.cex = 0.6)


# -----------------------------
#Saturacao e idade estão altamente correlacionados. Retirar saturação pois idade
#é mais correlacionada com a variável target
base_treino$saturacao <- NULL

dados_cor<-cor(base_treino[c(
  "idade",
  "internacoes12meses",
  "consultas30dias",
  "fumante",
  "urgencia",
  "creatinina",
  "pressao_arterial",
  "glicemia",
  "despesas_hospitalares")
]
)

corrplot(dados_cor,method="color",type = "upper",tl.cex = 0.6)
## ----------------------------------------------------------------------
# Etapa 4: Ajustando os modelos
#Ajustando o primeiro modelo
modelo1 <- lm(despesas_hospitalares ~ ., data = base_treino)
summary(modelo1)

#retirando variáveis não significativas

modelo2 <- lm(despesas_hospitalares ~ .
              -creatinina
              -glicemia, data = base_treino)
summary(modelo2)

#retirando urgencia para testar

modelo3 <- lm(despesas_hospitalares ~ .
              -creatinina
              -glicemia
              -urgencia, data = base_treino)
summary(modelo3)

#Definindo funções de RMSE
rmse_model_treino <- function(modelo, base) {
  previsao <- predict(modelo)
  previsao_real <- cbind(previsao, base$despesas_hospitalares)
  colnames(previsao_real) <- c('Previsto','Real')
  previsao_real <- as.data.frame(previsao_real)
  #MSE
  mse <- mean((previsao_real$Previsto - previsao_real$Real)^2)
  #RMSE
  rmse <- mse^0.5
  rmse
}

rmse_model_teste <- function(modelo, base) {
  previsao <- predict(modelo, base)
  previsao_real <- cbind(previsao, base$despesas_hospitalares)
  colnames(previsao_real) <- c('Previsto','Real')
  previsao_real <- as.data.frame(previsao_real)
  #MSE
  mse <- mean((previsao_real$Previsto - previsao_real$Real)^2)
  #RMSE
  rmse <- mse^0.5
  rmse
}

#modelo1 não deve ser usado: VARIÁVEIS NÃO SIGNIFICATIVAS

rmse_model_treino(modelo1, base_treino)
rmse_model_teste(modelo1, base_teste)

rmse_model_treino(modelo2, base_treino)
rmse_model_teste(modelo2, base_teste)

rmse_model_treino(modelo3, base_treino)
rmse_model_teste(modelo3, base_teste)