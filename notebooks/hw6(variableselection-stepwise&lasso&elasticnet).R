### Question 11.1
# Using the crime data set uscrime.txt from Questions 8.2, 9.1, and 10.1, build a regression model using:
# Stepwise regression
# Lasso
# Elastic net
# For Parts 2 and 3, remember to scale the data first – otherwise, the regression coefficients will be on different scales and the constraint won’t have the desired effect. 
# For Parts 2 and 3, use the glmnet function in R.  

### Solution 11.1
## [1] Stepwise Regression
library(MASS)
uscrime <- read.table("uscrime.txt", header = TRUE)

# 1. Full model / Null model 정의
full_model <- lm(Crime ~ ., data = uscrime)
null_model <- lm(Crime ~ 1, data = uscrime)

# 2. Stepwise 회귀 (양방향)
step_model <- stepAIC(
  object = null_model,
  scope = list(lower = null_model, upper = full_model),
  direction = "both",
  trace = TRUE
)

# 3. 결과 출력
cat("\n=============================\nStepwise Regression Summary\n=============================\n")
summary(step_model)

cat("\n=============================\nSelected Variables\n=============================\n")
print(step_model$call)

# 4. 모델 진단
par(mfrow = c(2, 2))
plot(step_model)



## [2] Lasso Regression
library(glmnet)

# 1. 종속변수 (y) 와 독립변수 (x) 분리
y <- uscrime$Crime
x <- as.matrix(uscrime[, -which(names(uscrime) == "Crime")])  # data.frame → matrix 변환

# 2. 데이터 스케일링 (표준화)
x_scaled <- scale(x)

# 3. Lasso 회귀 (alpha = 1)
lasso_model <- glmnet(x_scaled, y, alpha = 1)

# 4. 교차검증으로 최적 λ 찾기
cv_lasso <- cv.glmnet(x_scaled, y, alpha = 1)

# 5. 최적 λ 확인
best_lambda <- cv_lasso$lambda.min
cat("\n=============================\nBest lambda (λ):", best_lambda, "\n=============================\n")

# 6. 최적 λ 기반 계수 추정
lasso_coef <- coef(cv_lasso, s = "lambda.min")

# 7. 결과 출력
cat("\n=============================\nLasso Coefficients\n=============================\n")
print(lasso_coef)

# 8. 시각화
par(mfrow = c(1,2))
plot(lasso_model, xvar = "lambda", main = "Lasso Paths")
plot(cv_lasso, main = "Cross-validation for Lasso")



## [3] Elastic Net Regression

# 1. 종속변수 (y) 와 독립변수 (x) 분리 (상동)
y <- uscrime$Crime
x <- as.matrix(uscrime[, -which(names(uscrime) == "Crime")])

# 2. 데이터 스케일링 (상동)
x_scaled <- scale(x)

# 3. Elastic Net 회귀 실행
# alpha = 0.5 → Lasso(1)와 Ridge(0)의 중간
elastic_model <- glmnet(x_scaled, y, alpha = 0.5)

# 4. 교차검증으로 최적 λ 찾기
cv_elastic <- cv.glmnet(x_scaled, y, alpha = 0.5)

# 5. 최적 λ 출력
best_lambda <- cv_elastic$lambda.min
cat("\n=============================\nBest lambda (λ):", best_lambda, "\n=============================\n")

# 6. 최적 λ에서의 계수 확인
elastic_coef <- coef(cv_elastic, s = "lambda.min")
cat("\n=============================\nElastic Net Coefficients (alpha = 0.5)\n=============================\n")
print(elastic_coef)

# 7. 시각화
par(mfrow = c(1,2))
plot(elastic_model, xvar = "lambda", main = "Elastic Net Paths (alpha=0.5)")

plot(cv_elastic, main = "Cross-validation for Elastic Net (alpha=0.5)")
