# Exercício 3
library(rpart) # para a construção da árvore de decisão
library(rpart.plot) # para a visualização gráfico da árvore

# Modelo
prepare_hold_out <- function(tbl, training_perc) {
  tbl_mixed <- tbl[sample(1:nrow(tbl)), ]
  nrow <- nrow(tbl_mixed)
  
  nrow_train <- ceiling(training_perc * nrow)
  data_trn <- tbl_mixed[1:nrow_train, ]
  data_tst <- tbl_mixed[(1+nrow_train):(nrow), ]
  
  list(training = data_trn, test = data_tst)
}

wine_split <- prepare_hold_out(wine, 0.8)
tree <- rpart(quality ~ fixed.acidity + volatile.acidity + citric.acid
              + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide
              + density + pH + sulphates + alcohol,
              data = wine_split$training)
rpart.plot(tree)

# Predição
qualidade_predita <- predict(tree, wine_split$test, type = "class")

table(wine_split$test$quality)
table(qualidade_predita)

confusion_matrix <- table(wine_split$test$quality, qualidade_predita)
confusion_matrix

TP <- confusion_matrix[2, 2]
TN <- confusion_matrix[1:1]
FP <- confusion_matrix[1, 2]
FN <- confusion_matrix[2]

miss_rate <- FN / (FN+TP)
