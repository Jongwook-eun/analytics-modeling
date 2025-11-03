#install.packages("mice")
library(mice)
data <- read.csv("breast-cancer-wisconsin.data.txt", header = FALSE)
colnames(data) <- c("Sample_code_number", 
                    "Clump_thickness", 
                    "Uniformity_of_cell_size", 
                    "Uniformity_of_cell_shape", 
                    "Marginal_adhesion", 
                    "Single_epithelial_cell_size", 
                    "Bare_nuclei", 
                    "Bland_chromatin", 
                    "Normal_nucleoli", 
                    "Mitoses", 
                    "Class")
# "?"를 결측치로 변환
data[data == "?"] <- NA
# 수치형 변환
data[, 2:11] <- lapply(data[, 2:11], as.numeric)
# 결측치 확인
colSums(is.na(data))


## (1)최빈값 대체 
# Bare_nuclei는 1~10 범위의 이산형 정수이므로 평균보다 최빈값(mode) 으로 대체하는 것이 적절
# 최빈값(mode) 계산
mode_value <- as.numeric(names(sort(table(data$Bare_nuclei), decreasing = TRUE))[1])
# 결측치 대체
data_meanmode <- data
data_meanmode$Bare_nuclei[is.na(data_meanmode$Bare_nuclei)] <- mode_value
# 확인
mode_value
summary(data_meanmode$Bare_nuclei)


## (2)회귀 기반 대체 (Regression Imputation)
# 회귀 기반 대체: norm.predict
imp_reg <- mice(data, method = "norm.predict", m = 1, maxit = 5, seed = 42)
data_reg <- complete(imp_reg)
# 확인
imp_reg$imp$Bare_nuclei
summary(data_reg$Bare_nuclei)


## (3)회귀+교란(perturbation) 대체
# 회귀+교란(norm.nob)
imp_reg_perturb <- mice(data, method = "norm.nob", m = 1, maxit = 5, seed = 42)
data_reg_perturb <- complete(imp_reg_perturb)
# 확인
imp_reg_perturb$imp$Bare_nuclei
summary(data_reg_perturb$Bare_nuclei)
