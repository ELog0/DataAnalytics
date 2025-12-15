##########################################
### Principal Component Analysis (PCA) ###
##########################################

## load libraries
library(ggplot2)
library(ggfortify)
library(GGally)
library(e1071)
library(class)
library(psych)
library(readr)

## set working directory so that files can be referenced without the full path

## read dataset
wine <- read_csv("wine.data", col_names = FALSE)

## set column names
names(wine) <- c("Type","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid Phenols","Proanthocyanins","Color Intensity","Hue","Od280/od315 of diluted wines","Proline")

## inspect data frame
head(wine)

## change the data type of the "Type" column from character to factor
####
# Factors look like regular strings (characters) but with factors R knows 
# that the column is a categorical variable with finite possible values
# e.g. "Type" in the Wine dataset can only be 1, 2, or 3
####

wine$Type <- as.factor(wine$Type)


## visualize variables
pairs.panels(wine[,-1],gap = 0,bg = c("red", "yellow", "blue")[wine$Type],pch=21)

ggpairs(wine, ggplot2::aes(colour = Type))

###

wine.pca <- princomp(wine[,-1], cor = TRUE)

autoplot(wine.pca, data = wine, colour = "Type")

pc1_contrib <- sort(abs(wine.pca$loadings[,1]), decreasing = TRUE)
pc2_contrib <- sort(abs(wine.pca$loadings[,2]), decreasing = TRUE)

pc1_contrib
pc2_contrib

s.train <- sample(nrow(wine), size = 0.7 * nrow(wine))

wine.train <- wine[s.train, ]
wine.test  <- wine[-s.train, ]

train_x <- scale(wine.train[,-1])
test_x  <- scale(
  wine.test[,-1],
  center = attr(train_x, "scaled:center"),
  scale  = attr(train_x, "scaled:scale")
)

knn.all <- knn(train_x, test_x, cl = wine.train$Type, k = 5)

table(knn.all, wine.test$Type, dnn=list('predicted','actual'))

pc.scores <- as.data.frame(wine.pca$scores)

pc.train <- pc.scores[s.train, 1:2]
pc.test  <- pc.scores[-s.train, 1:2]

knn.pca <- knn(pc.train, pc.test, cl = wine.train$Type, k = 5)

table(knn.pca, wine.test$Type, dnn=list('predicted','actual'))

metrics <- function(tab) {
  precision <- diag(tab) / rowSums(tab)
  recall <- diag(tab) / colSums(tab)
  f1 <- 2 * precision * recall / (precision + recall)
  data.frame(Precision = precision, Recall = recall, F1 = f1)
}

tab.all <- table(knn.all, wine.test$Type)
tab.pca <- table(knn.pca, wine.test$Type)

metrics(tab.all)
metrics(tab.pca)
