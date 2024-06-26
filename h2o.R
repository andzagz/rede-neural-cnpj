library(h2o)
library(readxl)
library(caret)
library(dplyr)

# Carregar os dados
dados <- read_excel("dados_exportadosidlimpo.xlsx")


# Definir as sementes aleatórias para reprodutibilidade
set.seed(123)

# Estratificar os dados em treino e teste
index <- createDataPartition(dados$CR, p = 0.8, list = FALSE)
dados_treino <- dados[index, ]
dados_teste <- dados[-index, ]

# Converter a coluna CNPJ para character na base de teste
dados_teste$ID <- as.character(dados_teste$ID)

# Iniciar o cluster do H2O
h2o.init()

# Converter os dados de treino e teste para o formato H2O
dados_treino_h2o <- as.h2o(dados_treino)
dados_teste_h2o <- as.h2o(dados_teste)

# Definir as colunas preditoras e a coluna de resposta
preditoras <- setdiff(names(dados_treino_h2o), c("ID", "CR"))
resposta <- "CR"

# Converter as colunas preditoras para fator
for (coluna in preditoras) {
  dados_treino_h2o[[coluna]] <- as.factor(dados_treino_h2o[[coluna]])
  dados_teste_h2o[[coluna]] <- as.factor(dados_teste_h2o[[coluna]])
}

# Converter a coluna de resposta para fator
dados_treino_h2o[[resposta]] <- as.factor(dados_treino_h2o[[resposta]])
dados_teste_h2o[[resposta]] <- as.factor(dados_teste_h2o[[resposta]])

# Treinar o modelo de rede neural
modelo <- h2o.deeplearning(
  x = preditoras,
  y = resposta,
  training_frame = dados_treino_h2o,
  activation = "TanhWithDropout",
  hidden = c(10, 10, 10),
  epochs = 10
)

# Fazer previsões para a base de teste
previsoes <- h2o.predict(modelo, dados_teste_h2o)

previsoes <- as.data.frame(previsoes)
# View(previsoes)

library(openxlsx)

write.xlsx(previsoes,"previsoesh2o.xlsx")

