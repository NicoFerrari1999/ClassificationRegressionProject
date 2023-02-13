install.packages("caTools")
install.packages("dplyr")
install.packages("scatterplot3d")
library(caret)
library(caTools)
library(ggplot2)
library(dplyr)
library(scatterplot3d)

Possum_Dataset <- read.csv("C:\\Users\\ferra\\Desktop\\Progetto\\Regression\\possum.csv")
Possum_Dataset <- na.omit(Possum_Dataset)
Possum_Dataset$sex <- ifelse(Possum_Dataset$sex == "m", 1, 0)
Possum_Dataset$Pop <- ifelse(Possum_Dataset$Pop == "Vic", 1, 0)
n_iter <- 10

# ---------- OLS ----------
set.seed(123)
index <- createDataPartition(Possum_Dataset$totlngth, p = 0.7, list = FALSE)
Train_Possum <- Possum_Dataset[index, ]
Test_Possum <- Possum_Dataset[-index, ]

mae_values <- numeric(n_iter)
rmse_values <- numeric(n_iter)

for(i in 1:n_iter) {

  control <- trainControl(method = "cv", number = 5, verboseIter = TRUE)
  model <- train(totlngth ~ age + Pop + sex, data = Train_Possum, method = "lm", trContol = control)

  pred = predict(model, Test_Possum)


  mae_values[i] <- mean(abs(pred - Test_Possum$totlngth))
  rmse_values[i] <- sqrt(mean((pred - Test_Possum$totlngth)^2))

}

ggplot(Train_Possum, aes(age, totlngth)) + geom_point(shape = 16, color = "red") + geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Age & Length") +
  xlab("Age") +
  ylab("Length")

ggplot(Train_Possum, aes(Pop, totlngth)) + geom_point(shape = 16, color = "red") + geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Pop & Length") +
  xlab("Pop") +
  ylab("Length")

ggplot(Train_Possum, aes(sex, totlngth)) + geom_point(shape = 16, color = "red") + geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Sex & Length") +
  xlab("Sex") +
  ylab("Length")

mean_mae <- mean(mae_values)
mean_rmse <- mean(rmse_values)

print(paste("Mean of MAE: ", mean_mae))
print(paste("Mean of RMSE: ", mean_rmse))


# ---------- OLS WITHOUT OUTLIERS ----------
model_cook <- lm(totlngth ~ age + Pop + sex, data = Possum_Dataset)
cooks_distance <- cooks.distance(model_cook)
threshold <- 4 * mean(cooks_distance, na.rm = TRUE)
outliers <- which(cooks_distance > threshold)
Possum_Dataset_Clear <- Possum_Dataset[-outliers, ]

set.seed(123)
index2 <- createDataPartition(Possum_Dataset_Clear$totlngth, p = 0.7, list = FALSE)
Train_Possum_Clear <- Possum_Dataset_Clear[index2, ]
Test_Possum_Clear <- Possum_Dataset_Clear[-index2, ]

mae_values_clear <- numeric(n_iter)
rmse_values_clear <- numeric(n_iter)

for(i in 1:n_iter) {
  
  control_clear <- trainControl(method = "cv", number = 5, verboseIter = TRUE)
  control_clear
  model_clear <- train(totlngth ~ age + Pop + sex, data = Train_Possum_Clear, method = "lm", trContol = control)
  
  pred_clear = predict(model_clear, Test_Possum_Clear)
  
  mae_values_clear[i] <- mean(abs(pred_clear - Test_Possum_Clear$totlngth))
  rmse_values_clear[i] <- sqrt(mean((pred_clear - Test_Possum_Clear$totlngth)^2))
  
}

ggplot(Train_Possum_Clear, aes(age, totlngth)) + geom_point(shape = 16, color = "red") + geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Age & Length Cleaned") +
  xlab("Age Cleaned") +
  ylab("Length Cleaned")

ggplot(Train_Possum_Clear, aes(Pop, totlngth)) + geom_point(shape = 16, color = "red") + geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Pop & Length Cleaned") +
  xlab("Pop Cleaned") +
  ylab("Length Cleaned")

ggplot(Train_Possum_Clear, aes(sex, totlngth)) + geom_point(shape = 16, color = "red") + geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Sex & Length Cleaned") +
  xlab("Sex Cleaned") +
  ylab("Length Cleaned")

mean_mae_clear <- mean(mae_values_clear)
mean_rmse_clear <- mean(rmse_values_clear)

print(paste("Mean of MAE: ", mean_mae_clear))
print(paste("Mean of RMSE: ", mean_rmse_clear))
