# 🔄 Limpa o ambiente do R (remove todos os objetos da memória)
rm(list = ls())

# 🧹 Limpa o console (simula um "clear screen")
cat("\014")

# 📦 Instala os pacotes necessários (se ainda não estiverem instalados)
if (!require("caret")) install.packages("caret", dependencies = TRUE)  # para treinar modelos de ML
if (!require("ggplot2")) install.packages("ggplot2")                   # para gráficos
if (!require("readr")) install.packages("readr")                       # para leitura de arquivos .csv

# 📚 Carrega os pacotes na sessão
library(caret)
library(ggplot2)
library(readr)

# 📁 Carrega o dataset iris a partir do arquivo CSV
# Certifique-se de que o arquivo "iris.csv" está na mesma pasta do script
iris <- read_csv("iris.csv")

# 👀 Exibe as primeiras linhas e resumo estatístico dos dados
print(head(iris))
summary(iris)

# 🔄 Converte a variável alvo (Species) para tipo fator (necessário para classificação)
iris$Species <- as.factor(iris$Species)

# 🔀 Divide os dados em treino e teste (80% treino, 20% teste)
set.seed(123)  # garante reprodutibilidade
trainIndex <- createDataPartition(iris$Species, p = 0.8, list = FALSE)
trainData <- iris[trainIndex, ]  # dados de treino
testData  <- iris[-trainIndex, ] # dados de teste

# 🤖 Treina um modelo de machine learning usando KNN (K-Nearest Neighbors)
modelo <- train(
  Species ~ .,                     # fórmula: Species em função de todas as outras variáveis
  data = trainData,                # usa os dados de treino
  method = "knn",                  # algoritmo KNN
  trControl = trainControl(method = "cv", number = 10)  # validação cruzada com 10 folds
)

# 📈 Faz previsões no conjunto de teste
predicoes <- predict(modelo, newdata = testData)

# ✅ Avalia o desempenho do modelo usando uma matriz de confusão
# Aqui a função compara os valores reais (testData$Species) com os previstos (predicoes)
matriz <- confusionMatrix(
  factor(predicoes, levels = levels(testData$Species)),  # previsões como fator
  factor(testData$Species)                               # valores reais como fator
)
print(matriz)  # exibe as métricas como acurácia, sensibilidade, etc.

# 🎨 Visualiza as previsões com um gráfico de dispersão
ggplot(testData, aes(x = Petal.Length, y = Petal.Width, color = predicoes)) +
  geom_point(size = 3) +  # cada ponto representa uma flor
  labs(
    title = "Classificação das Espécies com KNN",  # título do gráfico
    x = "Comprimento da Pétala",                   # eixo X
    y = "Largura da Pétala"                        # eixo Y
  ) +
  theme_minimal()  # estilo de gráfico limpo e minimalista
