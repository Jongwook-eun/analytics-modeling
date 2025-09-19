### Question 3.1
# KNN Classification: CV + Holdout
library(kknn)
data <- read.table("credit_card_data-headers.txt", header = TRUE)
data$R1 <- factor(as.numeric(as.character(data$R1)), levels = c(0, 1))

# (a) 10-fold Cross-Validation to choose best K : K=5 or 10 is usually used (I used K=10)
set.seed(123)
n <- nrow(data)
K <- 10
k.values <- seq(1, 30, by = 2)

# Stratified K-fold (preserve class ratio)
make_stratified_folds <- function(y, K = 10, seed = 20250831) {
  set.seed(seed)
  y <- factor(y)
  idx_list <- split(seq_along(y), y)
  fold <- integer(length(y))
  for (cls in names(idx_list)) {
    ids <- idx_list[[cls]]
    fold_ids <- sample(rep(1:K, length.out = length(ids)))
    fold[ids] <- fold_ids
  }
  fold
}
fold_id <- make_stratified_folds(data$R1, K, seed = 20250831)

cv_acc <- numeric(length(k.values))

for (j in seq_along(k.values)) {
  preds <- factor(rep(NA, n), levels = c(0, 1))
  for (f in 1:K) {  # <<< K-fold CV Loop >>>
    idx.test  <- which(fold_id == f)
    idx.train <- setdiff(1:n, idx.test)
    fit <- kknn(
      R1 ~ .,
      train = data[idx.train, ],
      test  = data[idx.test,  ],
      k = k.values[j],
      scale = TRUE
    )
    p1 <- fit$prob[, "1"]                      # probability of class 1
    preds[idx.test] <- ifelse(p1 > 0.5, "1", "0")  # <- use character, not numeric
  }
  cv_acc[j] <- mean(preds == data$R1)          # accuracy of all folds
}

cv_result <- data.frame(k = k.values, cv_accuracy = round(cv_acc, 4))
print(cv_result)
best_k_cv <- cv_result$k[which.max(cv_result$cv_accuracy)]
cat(">> (a) Best k by 10-fold CV =", best_k_cv,
    " | CV Accuracy =", max(cv_result$cv_accuracy), "\n\n")


# (b) Train / Validation / Test split => final model selection & evaluation
set.seed(2025)
idx <- sample(1:n)
i_tr <- idx[1:round(0.6*n)]
i_va <- idx[(round(0.6*n)+1):(round(0.8*n))]
i_te <- idx[(round(0.8*n)+1):n]

train <- data[i_tr, ]
valid <- data[i_va, ]
test  <- data[i_te, ]

va_acc <- numeric(length(k.values))
for (j in seq_along(k.values)) {
  fit <- kknn(R1 ~ ., train = train, test = valid, k = k.values[j], scale = TRUE)
  p1  <- fit$prob[, "1"]
  pred <- ifelse(p1 > 0.5, "1", "0")           # <- character labels
  va_acc[j] <- mean(factor(pred, levels = c(0,1)) == valid$R1)
}

best_k_holdout <- k.values[which.max(va_acc)]
cat(">> (b) Best k on Validation =", best_k_holdout,
    " | Validation Accuracy =", max(round(va_acc, 4)), "\n")

# Train+Valid retrain → Test evaluation
final.fit <- kknn(R1 ~ ., train = rbind(train, valid), test = test,
                  k = best_k_holdout, scale = TRUE)
p1_test <- final.fit$prob[, "1"]
pred_test <- ifelse(p1_test > 0.5, "1", "0")

test_acc <- mean(factor(pred_test, levels = c(0,1)) == test$R1)
cat(">> (b) Test Accuracy =", round(test_acc, 4), "\n")

# Final test confusion matrix
cat("\nConfusion Matrix (Test):\n")
print(table(True = test$R1, Pred = factor(pred_test, levels = c(0, 1))))



### Question 4.2
iris <- read.table("iris.txt", header=TRUE)

X <- scale(iris[,1:4])   # predictors
y <- iris$Species        # for evaluation

k <- 3
km <- kmeans(X, centers=k, nstart=50)

tab <- table(km$cluster, y)
print(tab)

# # Simple accuracy (assuming each cluster is classified by its majority class)
simple_acc <- sum(apply(tab, 1, max)) / nrow(iris)
cat("Simple accuracy =", round(simple_acc,4), "\n")