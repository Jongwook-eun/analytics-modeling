### Question 8.2
# Using crime data from http://www.statsci.org/data/general/uscrime.txt  (file uscrime.txt, description at http://www.statsci.org/data/general/uscrime.html ), use regression (a useful R function is lm or glm) to predict the observed crime rate in a city with the following data:(M = 14.0, So = 0, Ed = 10.0, Po1 = 12.0, Po2 = 15.5, LF = 0.640, M.F = 94.0, Pop = 150, NW = 1.1, U1 = 0.120, U2 = 3.6, Wealth = 3200, Ineq = 20.1, Prob = 0.04, Time = 39.0)
# Show your model (factors used and their coefficients), the software output, and the quality of fit. 
# Note: that because there are only 47 data points and 15 predictors, you’ll probably notice some overfitting.  We’ll see ways of dealing with this sort of problem later in the course.)

### Solution 8.2
library(broom)

df = read.table("uscrime.txt", header = TRUE, check.names = FALSE)
str(df)
head(df)
df$So=as.factor(df$So)

lm.res = lm(Crime~.,data=df)
lm.sum = summary(lm.res)
print(lm.sum)

# 1) Show model predictors & coefficients & 95% CI
tidy(lm.res, conf.int = TRUE)

# 2) Quality of fit
r2      = lm.sum$r.squared
adj_r2  = lm.sum$adj.r.squared
rmse    = sigma(lm.res)              
aic_val = AIC(lm.res)
bic_val = BIC(lm.res)
c(R2 = r2, Adj_R2 = adj_r2, RMSE = rmse, AIC = aic_val, BIC = bic_val)

# 3) Diagnostic plots
par(mfrow=c(2,2))
plot(lm.res)

# 4) New-city prediction (mean + CI + PI)
new.data = data.frame(
  M = 14.0,
  So = factor(0, levels=c(0,1)),
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
pred_conf = predict(lm.res, newdata = new.data, interval = "confidence", level = 0.95)
pred_pred = predict(lm.res, newdata = new.data, interval = "prediction", level = 0.95)
cbind(pred_conf, pred_pred)

