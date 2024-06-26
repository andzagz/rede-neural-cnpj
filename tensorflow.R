# Instale e carregue o pacote tensorflow
# install.packages("tensorflow")
library(tensorflow)
library(keras)
library(nnet)
library(caret)
library(readxl)
library(dplyr)
library(openxlsx)
library(reticulate)


# Carregar os dados
dados <- read_excel("dados_exportados.xlsx")


# Definir as sementes aleatórias para reprodutibilidade
set.seed(123)

dados[,5:7] <- lapply(dados[,5:7],factor)


# # Estratificar os dados em treino e teste
# index <- createDataPartition(dados$Produto, p = 0.8, list = FALSE)
# dados_treino <- dados[index, ]
# dados_teste <- dados[-index, ]

# produto_encoded <- model.matrix(~ Produto - 1, data = dados)
regional_encoded <- model.matrix(~ Regional - 1, data = dados)
porte_encoded <- model.matrix(~ Porte.RFB - 1, data = dados)
SC_encoded <- model.matrix(~ SC.Competitiva -1, data = dados)


# Combinar dados codificados com outras variáveis
x_encoded <- cbind(regional_encoded, porte_encoded, SC_encoded, dados[, -which(names(dados) %in% c("CR", "Valor.Contrato.Total", "Regional", "Porte.RFB", "SC.Competitiva"))])
y_encoded <- model.matrix(~ CR - 1, data = dados)

# Verifique as dimensões dos dados de entrada
print(dim(x_encoded))

# Definir a arquitetura do modelo
output_shape <- dim(y_encoded)[2]

modelo <- keras_model_sequential() %>%
  layer_input(shape = c(input_shape)) %>%
  layer_dense(units = 8, activation = 'relu') %>%
  layer_dense(units = output_shape, activation = 'softmax')

# Compile o modelo
modelo %>% compile(
  loss = 'categorical_crossentropy',  # Usando "categorical_crossentropy" para classificação multi-classe
  optimizer = optimizer,
  metrics = c('accuracy')
)

# Treine o modelo
historico <- modelo %>% fit(
  x = x_encoded,
  y = y_encoded,
  epochs = 100,
  batch_size = 1,
  validation_split = 0.2
)


# Obter previsões nos dados de validação
previsoes_validacao <- predict(modelo, x[,-which(names(dados) %in% c("compras", "id_cliente"))])

# Avaliar o desempenho do modelo nos dados de validação
resultado_validacao <- modelo %>% evaluate(
  x = x[,-which(names(dados) %in% c("compras", "id_cliente"))],
  y = y
)
resultado_validacao
