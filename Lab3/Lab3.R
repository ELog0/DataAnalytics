library(class)
library(cluster)
library(factoextra)

set.seed(123)

# remove rings so it is not used as a predictor
abalone.data <- abalone.data[, !(names(abalone.data) %in% "rings")]

# create training and testing sets
s.train <- sample(nrow(abalone.data), size = 0.7 * nrow(abalone.data))
abalone.train <- abalone.data[s.train, ]
abalone.test  <- abalone.data[-s.train, ]

# check and use the actual column names from your file
names(abalone.data)

# helper: scale using training set statistics
scale_data <- function(train_x, test_x) {
  train_s <- scale(train_x)
  test_s  <- scale(
    test_x,
    center = attr(train_s, "scaled:center"),
    scale  = attr(train_s, "scaled:scale")
  )
  list(train = train_s, test = test_s)
}

# kNN MODEL 1 
features_1 <- c("length", "diameter", "height")

scaled_1 <- scale_data(abalone.train[, features_1], abalone.test[, features_1])

knn1.predicted <- knn(
  scaled_1$train,
  scaled_1$test,
  cl = abalone.train$age.group,
  k = 5
)


acc1 <- sum(knn1.predicted == abalone.test$age.group) / length(knn1.predicted)
acc1

# kNN MODEL 2 (subset of features 2)
features_2 <- c("whole_weight", "shucked_wieght", "viscera_wieght", "shell_weight")

scaled_2 <- scale_data(abalone.train[, features_2], abalone.test[, features_2])

knn2.predicted <- knn(
  scaled_2$train,
  scaled_2$test,
  cl = abalone.train$age.group,
  k = 5
)

table(knn2.predicted, abalone.test$age.group, dnn = list("predicted", "actual"))

acc2 <- sum(knn2.predicted == abalone.test$age.group) / length(knn2.predicted)
acc2


# Find optimal k for the better model
best.train <- scaled_2$train
best.test  <- scaled_2$test

k.values <- seq(1, 25, by = 2)
accuracy <- numeric(length(k.values))

for (i in seq_along(k.values)) {
  pred <- knn(best.train, best.test, cl = abalone.train$age.group, k = k.values[i])
  accuracy[i] <- sum(pred == abalone.test$age.group) / length(pred)
}

k.values[which.max(accuracy)]
max(accuracy)


#k-means model 


features_best <- c(
  "whole_weight",
  "shucked_wieght",
  "viscera_wieght",
  "shell_weight"
)

abalone.cluster <- abalone.data[, features_best]

# scale data (required for distance-based clustering)
abalone.cluster <- scale(abalone.cluster)

# K-MEANS: find optimal K
k_values <- 2:10
sil_kmeans <- numeric(length(k_values))

for (i in seq_along(k_values)) {
  km <- kmeans(abalone.cluster, centers = k_values[i], nstart = 25)
  sil <- silhouette(km$cluster, dist(abalone.cluster))
  sil_kmeans[i] <- mean(sil[, 3])
}

best_k_kmeans <- k_values[which.max(sil_kmeans)]
best_k_kmeans

# Fit final K-Means model
kmeans_final <- kmeans(abalone.cluster, centers = best_k_kmeans, nstart = 25)

# Silhouette plot for optimal K-Means
fviz_silhouette(
  silhouette(kmeans_final$cluster, dist(abalone.cluster))
)


# PAM: find optimal K
sil_pam <- numeric(length(k_values))

for (i in seq_along(k_values)) {
  pam_fit <- pam(abalone.cluster, k = k_values[i])
  sil_pam[i] <- pam_fit$silinfo$avg.width
}

best_k_pam <- k_values[which.max(sil_pam)]
best_k_pam

# Fit final PAM model
pam_final <- pam(abalone.cluster, k = best_k_pam)

# Silhouette plot for optimal PAM
fviz_silhouette(
  silhouette(pam_final$clustering, dist(abalone.cluster))
)