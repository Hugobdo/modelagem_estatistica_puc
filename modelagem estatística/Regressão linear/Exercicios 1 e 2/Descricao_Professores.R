# Prevendo o consumo de professores

# ------------------------------------------
# Etapa 1 - Verificando  os dados 
library(readxl)
professores <- read_excel("EXERCICIO1_Descricao_Professores.xls")
View(professores)


# ------------------------------------------
# Etapa 2 - Separando em base de treino e teste

#install.packages("caret", dependencies = TRUE)
library(caret)

set.seed(50)
indice_treino = createDataPartition(y=professores$horas_totais, p=0.7, list=FALSE)
base_treino = professores[indice_treino, ]
base_teste = professores[-indice_treino, ]
View(base_treino)
View(base_teste)

# ------------------------------------------
# Etapa 3: Explorando e Preparando os Dados

# Calculando a correlação(para verificar se existe correlação com a variável resposta)
#Valores
str(base_treino)
dados_cor<-cor(base_treino[c("tempo_casa","disciplina","alunos","horas_sala_aula","horas_extra_classe","nota_professor","horas_totais")])
dados_cor

#Gráficos
#install.packages("corrplot")
require(corrplot)

corrplot(dados_cor,tl.cex = 0.6)
corrplot(dados_cor,method="color",tl.cex = 0.5)

corrplot(dados_cor,type = "upper",tl.cex = 0.6)
corrplot(dados_cor,method="color",type = "upper",tl.cex = 0.6)

# -----------------------------
#Alunos, disciplina e horas_sala_aula estão altamente correlacionados
#Escolhi manter disciplina por possuir maior correlação com a variável target 
base_treino$alunos <- NULL

dados_cor<-cor(base_treino[c("tempo_casa","disciplina","horas_sala_aula","horas_extra_classe","nota_professor","horas_totais")])
corrplot(dados_cor,method="color",type = "upper",tl.cex = 0.6)

base_treino$horas_sala_aula <- NULL
dados_cor<-cor(base_treino[c("tempo_casa","disciplina","horas_extra_classe","nota_professor","horas_totais")])
corrplot(dados_cor,method="color",type = "upper",tl.cex = 0.6)


## ----------------------------------------------------------------------
# Etapa 4: Ajustando os modelos
#Ajustando o primeiro modelo
modelo1 <- lm(horas_totais ~ ., data = base_treino)
summary(modelo1)

#retirando variáveis não significativas

base_treino$prof_asst_iv <- ifelse(base_treino$cargo == "ASSISTENTE IV", 1, 0)
base_teste$prof_asst_iv <- ifelse(base_teste$cargo == "ASSISTENTE IV", 1, 0)
modelo2 <- lm(horas_totais ~ .-titulacao-cargo-tipo_contrato, data = base_treino)
summary(modelo2)

#somente numéricas

modelo3 <- lm(horas_totais ~ +tempo_casa +disciplina +horas_extra_classe +nota_professor, data = base_treino)
summary(modelo3)

#Prevendo as horas totais e testando modelos
rmse_model_treino <- function(modelo, base) {
  previsao <- predict(modelo)
  previsao_real <- cbind(previsao, base$horas_totais)
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
  previsao_real <- cbind(previsao, base$horas_totais)
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