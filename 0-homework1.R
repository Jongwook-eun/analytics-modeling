#Question 2.2.1-2.2.2 (SVM)

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


#Question 2.2.3 (KNN)

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
