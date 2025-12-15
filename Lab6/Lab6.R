library(tidyverse)
library(e1071)
set.seed(123)

dataset <- read_csv("NY-house-dataset.csv")
# Train / test split
idx <- sample(nrow(dataset), size = 0.7 * nrow(dataset))
train <- dataset[idx, ]
test  <- dataset[-idx, ]

# Linear Regression
lm.mod <- lm(PRICE ~ PROPERTYSQFT, data = train)
lm.pred <- predict(lm.mod, test)

# SVM (linear)
svm.lin <- svm(PRICE ~ PROPERTYSQFT, data = train, kernel = "linear")
svm.lin.pred <- predict(svm.lin, test)

# SVM (radial)
svm.rad <- svm(PRICE ~ PROPERTYSQFT, data = train, kernel = "radial")
svm.rad.pred <- predict(svm.rad, test)

# Error metrics
metrics <- function(actual, predicted) {
  err <- predicted - actual
  mae <- mean(abs(err))
  mse <- mean(err^2)
  rmse <- sqrt(mse)
  c(MAE = mae, MSE = mse, RMSE = rmse)
}

results <- rbind(
  Linear_Regression = metrics(test$PRICE, lm.pred),
  SVM_Linear        = metrics(test$PRICE, svm.lin.pred),
  SVM_Radial        = metrics(test$PRICE, svm.rad.pred)
)

results
