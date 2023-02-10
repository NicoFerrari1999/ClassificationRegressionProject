install.packages("caTools")
install.packages("tidyverse")
library(caTools)
library(tidyverse)
library(caret)
  
CC_Dataset <- read.csv("C:\\Users\\ferra\\Desktop\\Progetto\\Regression\\creditcard.csv")

# Out Adjusted R-squared is 0.2185.
model <- lm(Class ~ ., data = CC_Dataset)
summary(model)

# Is possible to notice some outliers.
par(mfrow = c(2, 2))
plot(model)

cooks_distance <- cooks.distance(model)

outlier_index <- which(cooks_distance>1)

CC_Dataset_Cleaned <- CC_Dataset[-outlier_index,]

model2 <- lm(Class ~ ., data = CC_Dataset_Cleaned)
summary(model2)

par(mfrow = c(2, 2))
plot(model2)
