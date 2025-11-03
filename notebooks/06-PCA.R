### Question 9.1
# Using the same crime data set uscrime.txt as in Question 8.2, apply Principal Component Analysis and then create a regression model using the first few principal components.  Specify your new model in terms of the original variables (not the principal components), and compare its quality to that of your solution to Question 8.2.  You can use the R function prcomp for PCA. (Note that to first scale the data, you can include scale. = TRUE to scale as part of the PCA function. 

### Solution 9.1
# ----- 0. Load data and define variables -----
df = read.table("uscrime.txt", header = TRUE, check.names = FALSE)
str(df)
head(df)
y = df$Crime                         # response variable
X = subset(df, select = -Crime)      # predictor variables (all except Crime)

# ----- 1. Fit a baseline OLS model (using all original variables) -----
lm.res = lm(Crime ~ ., data = df)
lm.sum = summary(lm.res)

# ----- 2. Perform PCA (standardize variables: mean=0, sd=1) ----- >> prcomp는 variable을 critical한 순서로 내림차순 자동배열함
pca = prcomp(X, center = TRUE, scale. = TRUE)
summary(pca)  # Check explained variance of each component

# ----- 3. Select optimal number of principal components (k) by minimizing BIC -----
p = ncol(X)
bic_vals = numeric(p)
aic_vals = numeric(p)
adjR2_vals = numeric(p)

for (k in 1:p) {
  pcs_k = pca$x[, 1:k, drop = FALSE]      # use first k principal components
  fit_k = lm(y ~ pcs_k)                   # fit linear model with k PCs
  bic_vals[k] = BIC(fit_k)                # store BIC
  aic_vals[k] = AIC(fit_k)                # store AIC
  adjR2_vals[k] = summary(fit_k)$adj.r.squared  # store adjusted R²
}

k_opt = which.min(bic_vals)               # choose k that minimizes BIC
cat("optimal k =", k_opt, "\n")

# ----- 4. Fit final PCR model with optimal number of components -----
pcs_opt = pca$x[, 1:k_opt, drop = FALSE]
colnames(pcs_opt) = paste0("PC", 1:k_opt)     # rename components as PC1, PC2, ...
fit_pcr = lm(y ~ ., data = as.data.frame(pcs_opt))
summary(fit_pcr)

# ----- 5. Convert coefficients back to original variable scale (unscaling) -----
# a: intercept, b: coefficients of principal components
a = coef(fit_pcr)[1]
b = coef(fit_pcr)[-1]                          # length: k_opt
L = pca$rotation[, 1:k_opt, drop = FALSE]      # loading matrix (p × k_opt)
center = pca$center                            # original variable means
scalev = pca$scale                             # original variable std devs

# w = L * b  (length p)
w = as.vector(L %*% b)

# Calculate coefficients on the original scale
beta_orig = w / scalev                         # coefficients for original variables
alpha_orig = a - sum(beta_orig * center)      # intercept on original scale

# Create a readable coefficient table
coef_orig = data.frame(
  Variable = colnames(X),
  Coef = beta_orig
)
coef_orig = rbind(
  data.frame(Variable = "(Intercept)", Coef = alpha_orig),
  coef_orig
)
print(coef_orig)

# ----- 6. Compare performance metrics: OLS vs PCR -----
rmse = function(obs, pred) sqrt(mean((obs - pred)^2))

# OLS (all original variables)
ols_sum = summary(lm.res)
ols_adjR2 = ols_sum$adj.r.squared
ols_AIC = AIC(lm.res)
ols_RMSE = rmse(y, predict(lm.res))

# PCR (k_opt PCs)
pcr_sum = summary(fit_pcr)
pcr_adjR2 = pcr_sum$adj.r.squared
pcr_AIC = AIC(fit_pcr)
pcr_RMSE = rmse(y, predict(fit_pcr))

comp = data.frame(
  Model = c("OLS (All vars)", paste0("PCR (", k_opt, " PCs)")),
  Adj_R2 = c(ols_adjR2, pcr_adjR2),
  AIC = c(ols_AIC, pcr_AIC),
  RMSE = c(ols_RMSE, pcr_RMSE)
)
print(comp)
#>> 돌려보면 R^2는 낮게, AIC/RMSE는 높게나와서 모델평가는 더 안좋아졌지만, PCA는 그럼에도 다중공선성이나 overfit을 어느정도 해결해주는 장점이 잇음

# ----- 7. Prediction for a new observation -----
new_city = data.frame(
  M = 14.0,
  So = 0,
  Ed = 10.0,
  Po1 = 12.0,
  Po2 = 15.5,
  LF = 0.640,
  M.F = 94.0,
  Pop = 150,
  NW = 1.1,
  U1 = 0.120,
  U2 = 3.6,
  Wealth = 3200,
  Ineq = 20.1,
  Prob = 0.04,
  Time = 39.0
)

# ----- 7-1. Direct prediction using original-scale coefficients -----
# (Only gives point estimate, cannot provide confidence/prediction intervals)
pred_new = as.numeric(alpha_orig + sum(beta_orig * new_city[colnames(X)]))
cat("Predicted Crime for the new city =", pred_new, "\n")

# ----- 7-2. Transform new observation into PCA space -----
new_scaled <- sweep(as.matrix(new_city[, colnames(X)]), 2, center, "-")  # center
new_scaled <- sweep(new_scaled, 2, scalev, "/")                         # scale
new_pcs <- new_scaled %*% L                                             # project into PC space

# ----- 7-3. Create a 1-row data frame with correct PC names -----
nd <- as.data.frame(new_pcs)
colnames(nd) <- paste0("PC", 1:k_opt)

# ----- 7-4. Prediction with confidence and prediction intervals -----
pred_ci <- predict(fit_pcr, newdata = nd, interval = "confidence", level = 0.95)    # CI for mean
pred_pi <- predict(fit_pcr, newdata = nd, interval = "prediction", level = 0.95)    # PI for individual

cbind(pred_ci, pred_pi)


