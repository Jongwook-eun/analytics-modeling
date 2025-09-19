### Question 5.1
# Outlier Test
library(outliers)
data<-read.table("uscrime.txt",header=T)
head(data)
y<-data[,ncol(data)]
grubbs.test(y)

### Question 6.2
## Change Detection(CUSUM approach)
# 1) 
data2 <- read.table("temps.txt", header = TRUE, check.names = FALSE)
head(data2) 
years <- names(data2)[-1]             
years_num <- as.numeric(sub("^X", "", years))

# 2) CUSUM Calculation
cusum_simple <- function(x) {
  x <- as.numeric(x)
  mu <- mean(x, na.rm = TRUE)
  s  <- sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) {
    return(list(cusum = rep(0, length(x)), cp = NA))
  }
  z <- (x - mu) / s           
  csum <- cumsum(z)           
  cp_idx <- which.max(abs(csum))  # The point CUSUM deviates the most = change-point
  list(cusum = csum, cp = cp_idx)
}

# 3) Year-by-Year CUSUM & Change-Point (End of Summer) Extraction
cp_index <- integer(length(years))
for (i in seq_along(years)) {
  y <- data2[[years[i]]]
  res <- cusum_simple(y)
  cp_index[i] <- res$cp
}

cp_df <- data.frame(
  Year = years_num,
  CP_Index = cp_index  # 1 = 7/1, ..., 123 ≈ 10/31
)
print(cp_df)

# 4) Visualization
plot(cp_df$Year, cp_df$CP_Index, type="b", pch=19,
     main="Unofficial Summer End (CUSUM change-point) by Year",
     xlab="Year", ylab="Change-point index (larger = later end)")
abline(lm(CP_Index ~ Year, data = cp_df), lty=2, lwd=2)

# 5) Trend check
jul_aug_mean <- sapply(years, function(col) {
  x <- data2[[col]]
  mean(x[1:62], na.rm = TRUE)  # First 62 days (July and August)
})
summer_df <- data.frame(
  Year = years_num,
  JulAugMean = as.numeric(jul_aug_mean)
)
print(summer_df)

plot(summer_df$Year, summer_df$JulAugMean, type="b", pch=19,
     main="Midsummer (July–August) Mean High by Year",
     xlab="Year", ylab="Mean High (°F)")
abline(lm(JulAugMean ~ Year, data = summer_df), lty=2, lwd=2)