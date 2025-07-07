# Instalar os pacotes (roda uma vez só se necessário)
# install.packages(c("caret", "randomForest", "e1071", "ggplot2", "ROSE"))

# Carregar as bibliotecas
library(caret)
library(randomForest)
library(e1071)
library(ggplot2)
library(ROSE)  # Para balanceamento

# Selecionar o arquivo CSV manualmente
caminho <- file.choose()

# Leitura do arquivo
dados <- read.csv(caminho, stringsAsFactors = TRUE)

# Limpeza e preparação dos dados
dados$customerID <- NULL
dados[dados == ""] <- NA
dados <- na.omit(dados)
dados$Churn <- as.factor(ifelse(dados$Churn == "Yes", 1, 0))

# Divisão em treino e teste
set.seed(123)
indice <- createDataPartition(dados$Churn, p = 0.7, list = FALSE)
treino <- dados[indice, ]
teste  <- dados[-indice, ]

# Balanceamento da base de treino com ROSE
set.seed(123)
treino_bal <- ROSE(Churn ~ ., data = treino)$data

# Ajuste de mtry usando tuneRF (opcional, pode demorar um pouco)
melhor_mtry <- tuneRF(
  treino_bal[, -which(names(treino_bal) == "Churn")],
  treino_bal$Churn,
  stepFactor = 1.5,
  ntreeTry = 100,
  improve = 0.01,
  trace = TRUE,
  plot = TRUE
)

# Pegando o melhor mtry encontrado (exemplo: 3)
mtry_melhor <- melhor_mtry[which.min(melhor_mtry[, 2]), 1]

# Treinamento com Random Forest usando o melhor mtry e ntree maior
set.seed(123)
modelo <- randomForest(Churn ~ ., data = treino_bal, ntree = 500, mtry = mtry_melhor)

# Previsões no conjunto de teste
predicoes <- predict(modelo, teste)

# Avaliação com Confusion Matrix
matriz <- confusionMatrix(predicoes, teste$Churn)
print(matriz)

# Gráfico das previsões corretas vs incorretas
resultados <- data.frame(
  Real = teste$Churn,
  Previsto = predicoes
)
resultados$Acerto <- ifelse(resultados$Real == resultados$Previsto, "Correto", "Incorreto")

ggplot(resultados, aes(x = Previsto, fill = Acerto)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Previsões do Modelo - Churn",
    x = "Churn Previsto",
    y = "Quantidade",
    fill = "Resultado"
  ) +
  theme_minimal()
