##########################################
### SVM Classification – Wine Dataset ###
##########################################

library(e1071)
library(class)

set.seed(123)


# Feature subset (from PCA)
# Top contributors to PC1 / PC2

features <- c(
  "Alcohol",
  "Flavanoids",
  "Color Intensity",
  "Proline"
)

wine.sub <- wine[, c("Type", features)]


# Train / test split
s.train <- sample(nrow(wine.sub), size = 0.7 * nrow(wine.sub))

wine.train <- wine.sub[s.train, ]
wine.test  <- wine.sub[-s.train, ]


# Scale predictors

train_x <- scale(wine.train[, -1])
test_x  <- scale(
  wine.test[, -1],
  center = attr(train_x, "scaled:center"),
  scale  = attr(train_x, "scaled:scale")
)

train_y <- wine.train$Type
test_y  <- wine.test$Type


# SVM – Linear kernel

tune.linear <- tune.svm(
  x = train_x,
  y = train_y,
  kernel = "linear",
  cost = 10^(-2:2)
)

svm.linear <- tune.linear$best.model

pred.linear <- predict(svm.linear, test_x)


# SVM – RBF kernel

tune.rbf <- tune.svm(
  x = train_x,
  y = train_y,
  kernel = "radial",
  cost = 10^(-2:2),
  gamma = 10^(-2:2)
)

svm.rbf <- tune.rbf$best.model

pred.rbf <- predict(svm.rbf, test_x)


# kNN classifier (same features)
knn.pred <- knn(
  train_x,
  test_x,
  cl = train_y,
  k = 5
)


# Contingency tables

tab.linear <- table(pred.linear, test_y)
tab.rbf    <- table(pred.rbf, test_y)
tab.knn    <- table(knn.pred, test_y)

tab.linear
tab.rbf
tab.knn


# Precision / Recall / F1
metrics <- function(tab) {
  precision <- diag(tab) / rowSums(tab)
  recall    <- diag(tab) / colSums(tab)
  f1        <- 2 * precision * recall / (precision + recall)
  data.frame(Precision = precision, Recall = recall, F1 = f1)
}

metrics(tab.linear)
metrics(tab.rbf)
metrics(tab.knn)
