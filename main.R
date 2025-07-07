# Limpar o ambiente
rm(list = ls())
cat("\014")

# Instalar pacotes (apenas na primeira vez)
if (!require("caret")) install.packages("caret", dependencies = TRUE)
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("readr")) install.packages("readr")

# Carregar pacotes
library(caret)
library(ggplot2)
library(readr)

# Carregar o dataset (certifique-se que iris.csv está na mesma pasta)
iris <- read_csv("iris.csv")

# Verificar os dados
print(head(iris))
summary(iris)

# Transformar a variável alvo em fator, se necessário
iris$Species <- as.factor(iris$Species)

# Separar dados em treino e teste (80% treino)
set.seed(123)
trainIndex <- createDataPartition(iris$Species, p = 0.8, list = FALSE)
trainData <- iris[trainIndex, ]
testData  <- iris[-trainIndex, ]

# Treinar modelo KNN
modelo <- train(
  Species ~ ., 
  data = trainData,
  method = "knn",
  trControl = trainControl(method = "cv", number = 10)
)

# Fazer previsões
predicoes <- predict(modelo, newdata = testData)

# Avaliar modelo com matriz de confusão
matriz <- confusionMatrix(
  factor(predicoes, levels = levels(testData$Species)),
  factor(testData$Species)
)
print(matriz)

# Visualizar as classificações
ggplot(testData, aes(x = Petal.Length, y = Petal.Width, color = predicoes)) +
  geom_point(size = 3) +
  labs(
    title = "Classificação das Espécies com KNN",
    x = "Comprimento da Pétala",
    y = "Largura da Pétala"
  ) +
  theme_minimal()
