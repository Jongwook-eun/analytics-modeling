### Question 7.2
# Using the 20 years of daily high temperature data for Atlanta (July through October) from Question 6.2 (file temps.txt), build and use an exponential smoothing model to help make a judgment of whether the unofficial end of summer has gotten later over the 20 years.  (Part of the point of this assignment is for you to think about how you might use exponential smoothing to answer this question. Feel free to combine it with other models if you’d like to. There’s certainly more than one reasonable approach.) 
# Note: in R, you can use either HoltWinters (simpler to use) or the smooth package’s es function (harder to use, but more general).  If you use es, the Holt-Winters model uses model=”AAM” in the function call (the first and second constants are used “A”dditively, and the third (seasonality) is used “M”ultiplicatively; the documentation doesn’t make that clear). 

### Solution 7.2
library(tidyverse)
library(zoo)
library(lubridate)
head(df)

# ---------- Parameters ----------
alpha_user      <- NA
win_days        <- 7
min_slope       <- -0.25
k_consecutive   <- 5
anchor_monthday <- "08-15"

# ---------- Load & reshape ----------
df <- read.table("temps.txt", header = TRUE, check.names = FALSE)

long <- df %>%
  pivot_longer(-DAY, names_to = "year", values_to = "temp") %>%
  mutate(
    year     = as.integer(sub("^X", "", year)),
    date     = parse_date_time(paste(DAY, year), orders = "d-b Y", locale = "C")
  ) %>%
  arrange(year, date) %>%
  group_by(year) %>%
  mutate(doy = yday(date)) %>%
  ungroup()

anchor_map <- long %>%
  distinct(year) %>%
  mutate(anchor_date = as.Date(paste0(year, "-", anchor_monthday)))

long <- long %>% left_join(anchor_map, by = "year")

# ---------- Function ----------
find_end_of_summer <- function(d) {
  hw <- if (is.na(alpha_user)) {
    HoltWinters(d$temp, beta = FALSE, gamma = FALSE)
  } else {
    HoltWinters(d$temp, beta = FALSE, gamma = FALSE, alpha = alpha_user)
  }
  
  smoothed <- c(NA, hw$fitted[, "xhat"])
  
  slope_win <- rollapply(
    smoothed, width = win_days, by = 1, align = "right",
    FUN = function(y) { if (any(is.na(y))) return(NA) else coef(lm(y ~ seq_along(y)))[2] },
    fill = NA
  )
  
  out <- d %>%
    mutate(smoothed = smoothed, slope = slope_win) %>%
    filter(date >= anchor_date)
  
  cond <- !is.na(out$slope) & out$slope <= min_slope
  runlen <- rle(cond)
  if (any(runlen$values & runlen$lengths >= k_consecutive)) {
    idx <- which(cond)[1]
    tibble(year = d$year[1], end_date = out$date[idx], end_doy = yday(out$date[idx]),
           alpha = unname(hw$alpha))
  } else {
    tibble(year = d$year[1], end_date = as.Date(NA), end_doy = NA,
           alpha = unname(hw$alpha))
  }
}

# ---------- Apply ----------
by_year <- long %>%
  group_by(year) %>%
  group_modify(~ find_end_of_summer(.x)) %>%
  ungroup()

print(by_year)

# ---------- Trend ----------
fit <- lm(end_doy ~ year, data = by_year, na.action = na.omit)
summary(fit)

# ---------- Plot ----------
ggplot(by_year, aes(x = year, y = end_doy)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "End of Summer DOY (Atlanta Jul–Oct)", x = "Year", y = "DOY") +
  theme_minimal()

