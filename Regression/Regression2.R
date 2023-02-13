install.packages("caTools")
install.packages("quantreg")
library(caTools)
library(quantreg)
library(ggplot2)

Possum_Dataset <- read.csv("C:\\Users\\ferra\\Desktop\\Progetto\\Regression\\possum.csv")
Possum_Dataset <- na.omit(Possum_Dataset)
Possum_Dataset$sex <- ifelse(Possum_Dataset$sex == "m", 1, 0)
Possum_Dataset$Pop <- ifelse(Possum_Dataset$Pop == "Vic", 1, 0)
n_iter <- 10

# ---------- QUANTILE REGRESSION ----------
set.seed(123)
index <- sample(nrow(Possum_Dataset), 0.7 * nrow(Possum_Dataset))
Train_Possum <- Possum_Dataset[index, ]
Test_Possum <- Possum_Dataset[-index, ]

mae_values <- numeric(n_iter)
rmse_values <- numeric(n_iter)
rsq_values <- numeric(n_iter)

for (i in 1:n_iter) {
  
  model <- rq(hdlngth ~ age + Pop + sex, data = Train_Possum, tau = 0.5)
  
  pred <- predict(model, newdata = Test_Possum)
  
  mae_values[i] <- mean(abs(pred - Test_Possum$hdlngth))
  rmse_values[i] <- sqrt(mean((pred - Test_Possum$hdlngth)^2))
  rsq_values[i] <- cor(pred, Test_Possum$hdlngth)^2
  
}

ggplot(Train_Possum, aes(age, hdlngth)) + geom_point(shape = 16, color = "red") + geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Age & Head length") +
  xlab("Age") +
  ylab("Head length")

ggplot(Train_Possum, aes(Pop, hdlngth)) + geom_point(shape = 16, color = "red") + geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Pop & Head length") +
  xlab("Pop") +
  ylab("Head length")

ggplot(Train_Possum, aes(sex, hdlngth)) + geom_point(shape = 16, color = "red") + geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Sex & Head length") +
  xlab("Sex") +
  ylab("Head length")

mean_mae <- mean(mae_values)
mean_rmse <- mean(rmse_values)
mean_rsq <- mean(rsq_values)

# ---------- QUANTILE REGRESSION WITHOUT OUTLIERS ----------
model_cook <- lm(hdlngth ~ age + Pop + sex, data = Possum_Dataset)
cooks_distance <- cooks.distance(model_cook)
threshold <- 4 * mean(cooks_distance, na.rm = TRUE)
outliers <- which(cooks_distance > threshold)
Possum_Dataset_Clear <- Possum_Dataset[-outliers, ]

set.seed(123)
index2 <- createDataPartition(Possum_Dataset_Clear$hdlngth, p = 0.7, list = FALSE)
Train_Possum_Clear <- Possum_Dataset_Clear[index2, ]
Test_Possum_Clear <- Possum_Dataset_Clear[-index2, ]

mae_values_clear <- numeric(n_iter)
rmse_values_clear <- numeric(n_iter)
rsq_values_clear <- numeric(n_iter)


for (i in 1:n_iter) {
  
  model_clear <- rq(hdlngth ~ age + Pop + sex, data = Train_Possum_Clear, tau = 0.5)
  
  pred_clear <- predict(model_clear, newdata = Test_Possum_Clear)
  
  mae_values_clear[i] <- mean(abs(pred_clear - Test_Possum_Clear$hdlngth))
  rmse_values_clear[i] <- sqrt(mean((pred_clear - Test_Possum_Clear$hdlngth)^2))
  rsq_values_clear[i] <- cor(pred_clear, Test_Possum_Clear$hdlngth)^2
  
}

ggplot(Train_Possum_Clear, aes(age, hdlngth)) + geom_point(shape = 16, color = "red") + geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Age & Head length Cleaned") +
  xlab("Age Cleaned") +
  ylab("Head length Cleaned")

ggplot(Train_Possum_Clear, aes(Pop, hdlngth)) + geom_point(shape = 16, color = "red") + geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Pop & Head length Cleaned") +
  xlab("Pop Cleaned") +
  ylab("Head length Cleaned")

ggplot(Train_Possum_Clear, aes(sex, hdlngth)) + geom_point(shape = 16, color = "red") + geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Sex & Head length Cleaned") +
  xlab("Sex Cleaned") +
  ylab("Head length Cleaned")

mean_mae_clear <- mean(mae_values_clear)
mean_rmse_clear <- mean(rmse_values_clear)
mean_rsq_clear <- mean(rsq_values_clear)

# ---------- COMPARISON BETWEEN OUTPUTS ----------

print("BEFORE OUTLIERS REMOTION")
print(paste("Mean of MAE: ", mean_mae))
print(paste("Mean of RMSE: ", mean_rmse))
print(paste("Mean of Pearson R-squared: ", mean_rsq))

print("AFTER OUTLIERS REMOTION")
print(paste("Mean of MAE: ", mean_mae_clear))
print(paste("Mean of RMSE: ", mean_rmse_clear))
print(paste("Mean of Pearson R-squared: ", mean_rsq_clear))
