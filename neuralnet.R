install.packages("neuralnet")

library(nnet)
library(readxl)
library(dplyr)
library(openxlsx)

tabela_completav2 <- data.frame(read_excel("tabela_completav2.xlsx"))

set.seed(0)

# transformação as.factor -------------------------------------------------


colunas_para_transformar <- 1:16 

tabela_completav2[, colunas_para_transformar] <- lapply(tabela_completav2[, colunas_para_transformar], as.factor)

colunas_para_transformar <- 20:46

tabela_completav2[, colunas_para_transformar] <- lapply(tabela_completav2[, colunas_para_transformar], as.factor)


# str(tabela_completav2)
# names(tabela_completav2)



dados <- c("CNPJ","Razão.Social", "Valor.Contrato.Total", "Produto", "Regional",
                        # "Filial", "Situação",
                        "Porte.RFB", "SC.Competitiva")  
                        #"Divisão")



dados <- tabela_completav2[, dados]

#View(dados)

any(is.na(dados))

dados <- na.omit(dados)

dados[, "Valor.Contrato.Total"] <- scale(dados[, "Valor.Contrato.Total"])



#str(dados)


# Fim do pré processamento ------------------------------------------------

#Train-Test Split
train_test_split_index <- 0.8 * nrow(dados)

train <- data.frame(dados[1:train_test_split_index,])
test <- data.frame(dados[(train_test_split_index+1): nrow(dados),])

#Padronizar dados para melhor performance
#Explicar apply



index = sample(1:nrow(data),round(0.70*nrow(data)))
train_data <- as.data.frame(scaled[index,])
test_data <- as.data.frame(scaled[-index,])


# Treinar a rede neural
modelo <- nnet(
  Produto ~ Valor.Contrato.Total + Regional + Porte.RFB + SC.Competitiva,
  data = dados,
  size = 1, # Número de neurônios na camada oculta
  linout = FALSE # Definir como FALSE para problemas de classificação multiclasse
)



# Fazer previsões no conjunto de teste
previsoes <- predict(modelo, newdata = dados_teste, type = "class")


# Calcular a acurácia
accuracy[fold] <- mean(previsoes == dados_teste$produto_recomendado)




# Visualizar o modelo
summary(modelo)
