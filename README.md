🌸 Classificação de Espécies com KNN (R)
Este projeto utiliza o algoritmo KNN (K-Nearest Neighbors) para classificar flores do dataset iris.

✅ O que o script faz
Limpa o ambiente e o console.

Instala e carrega os pacotes necessários (caret, ggplot2, readr).

Lê o arquivo iris.csv.

Prepara os dados (conversão para fator e divisão em treino/teste).

Treina um modelo KNN com validação cruzada.

Avalia o modelo com matriz de confusão.

Gera um gráfico com as previsões.

▶️ Como usar
Coloque o arquivo iris.csv na mesma pasta do script.

Rode o script no R ou RStudio.

r
Copiar
Editar
# Exemplo de execução do script
source("script_knn_iris.R")
📈 Saída esperada
Métricas de desempenho (acurácia, sensibilidade, etc.).

Gráfico de dispersão com as espécies previstas.

📝 Requisitos
R instalado

Pacotes: caret, ggplot2, readr (instalados automaticamente)