### Question 2.2
# The files credit_card_data.txt (without headers) and credit_card_data-headers.txt (with headers) contain a dataset with 654 data points, 6 continuous and 4 binary predictor variables.  It has anonymized credit card applications with a binary response variable (last column) indicating if the application was positive or negative. The dataset is the “Credit Approval Data Set” from the UCI Machine Learning Repository (https://archive.ics.uci.edu/ml/datasets/Credit+Approval) without the categorical variables and without data points that have missing values.

### Question 2.2.1 
# Using the support vector machine function ksvm contained in the R package kernlab, find a good classifier for this data. Show the equation of your classifier, and how well it classifies the data points in the full data set.  (Don’t worry about test/validation data yet; we’ll cover that topic soon.)
### Question 2.2.2
# You are welcome, but not required, to try other (nonlinear) kernels as well; we’re not covering them in this course, but they can sometimes be useful and might provide better predictions than vanilladot.
### Question 2.2.3
# Using the k-nearest-neighbors classification function kknn contained in the R kknn package, suggest a good value of k, and show how well it classifies that data points in the full data set.  Don’t forget to scale the data (scale=TRUE in kknn).

### Solution 2.2.1-2.2.2 (SVM)
data <- read.table("credit_card_data-headers.txt",header=T)
str(data)
data[,11]=as.factor(data[,11])
str(data)

library(kernlab)

X <- as.matrix(data[,1:10])
y <- data$R1

y

#I tried various C values to see if there is a significant change in terms of the model's predictability.
model <- ksvm(X,y,kernel='vanilladot',C=1,scaled=TRUE)
model2 <- ksvm(X,y,kernel='rbfdot',C=1,scaled=TRUE)
model3 <- ksvm(X,y,kernel='polydot',C=1,scaled=TRUE)

# Intercept (bias)
a0 = model@b
-a0

pred <- predict(model,X)
pred2 <- predict(model2,X)
pred3 <- predict(model3,X)

sum((pred==y) / nrow(data))
sum((pred2==y) / nrow(data))
sum((pred3==y) / nrow(data))

# Calculate coefficients (a1...am)
a <- colSums(model@xmatrix[[1]] * model@coef[[1]])
a


### Solution 2.2.3 (KNN)
library(kknn)

# numeric(0/1)
data$R1 <- as.numeric(as.character(data$R1))

n <- nrow(data)
k.values <- seq(1, 25, by = 2)
acc <- numeric(length(k.values))

for (j in seq_along(k.values)) {
  preds <- numeric(n)
  
  for (i in 1:n) {
    fit <- kknn(
      R1 ~ ., 
      train = data[-i, ], 
      test  = data[i, , drop = FALSE], 
      k = k.values[j],
      scale = TRUE
    )
    
    # fitted() returns probability of class=1 → threshold 0.5
    p <- fitted(fit)
    preds[i] <- ifelse(p > 0.5, 1, 0)
  }
  
  acc[j] <- mean(preds == data$R1)  # "predicted value VS real value"
}

results <- data.frame(k = k.values, accuracy = acc)
print(results)


