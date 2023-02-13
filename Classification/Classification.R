library(caTools)
library(class)
library(e1071)
library(caret)
library(Amelia)

HD_Dataset <- read.csv("C:\\Users\\Nico\\Desktop\\Progetto Morasca\\Classification\\heart.csv")

# Then we normalized the dataset so that the output remains unbiased.
normalize <- function(x){return ((x-min(x))/(max(x)-min(x)))}
HD_Dataset_Normalized <- as.data.frame(lapply(HD_Dataset, normalize))
n_iter <- 50

# We split the dataset into training set and testing set.
#set.seed(123)
index <- createDataPartition(HD_Dataset_Normalized$output, p = 0.7, list = FALSE)
Train_HD <- HD_Dataset_Normalized[index, 1:13]
Train_Label_HD <- HD_Dataset_Normalized[index, 14]
Test_HD <- HD_Dataset_Normalized[-index, 1:13]
Test_Label_HD <- HD_Dataset_Normalized[-index, 14]

Train_Label_HD <- as.factor(Train_Label_HD)
Test_Label_HD <- as.factor(Test_Label_HD)
Train_Label_HD <- factor(Train_Label_HD, levels = unique(c(Train_Label_HD, Test_Label_HD)))
Test_Label_HD <- factor(Test_Label_HD, levels = unique(c(Train_Label_HD, Test_Label_HD)))

accuracy_knn <- c()

# --------------- K-NN ---------------

for (i in 1:n_iter) {
  
  model <- knn(Train_HD, Test_HD, Train_Label_HD, k = i)
  
  cm <- confusionMatrix(model, Test_Label_HD)
  
  accuracy_knn[i] <- cm$overall[1]
  
}

plot(1:n_iter, accuracy_knn, type = "l", xlab = "K", ylab = "Accuracy", main = "Accuracy of KNN Classifier", col = "blue")
points(1:50, accuracy_knn, pch = 20, col = "red")
axis(1, at = seq(0, 50, by = 2), labels = seq(0, 50, by = 2))

jaccard_knn <- (cm$table[1, 1] / (cm$table[1, 1] + cm$table[1, 2] + cm$table[2, 1]))
jouden_knn <- ((cm$table[1, 1]/(cm$table[1, 1]+cm$table[2, 1])) - ((cm$table[1, 2])/(cm$table[1, 2]+cm$table[2, 2])))
markedness_knn <- ((cm$table[1, 1]/(cm$table[1, 1] + cm$table[1, 2]))-((cm$table[2, 1])/(cm$table[2, 1] + cm$table[2, 2])))

# --------------- NAIVE BAYES  ---------------

model_NB <- naiveBayes(Train_HD, Train_Label_HD)

pred <-predict(model_NB, Test_HD)

cm_NB <- confusionMatrix(pred, Test_Label_HD)

jaccard_NB <- (cm_NB$table[1, 1] / (cm_NB$table[1, 1] + cm_NB$table[1, 2] + cm_NB$table[2, 1]))
jouden_NB <- ((cm_NB$table[1, 1]/(cm_NB$table[1, 1]+cm_NB$table[2, 1])) - ((cm_NB$table[1, 2])/(cm_NB$table[1, 2]+cm_NB$table[2, 2])))
markedness_NB <- ((cm_NB$table[1, 1]/(cm_NB$table[1, 1] + cm_NB$table[1, 2]))-((cm_NB$table[2, 1])/(cm_NB$table[2, 1] + cm_NB$table[2, 2])))

# ---------- COMPARISON BETWEEN OUTPUTS ----------
print(paste("KNN CONFUSION MATRIX: "))
print(cm$table)
print(paste("NAIVE BAYES CONFUSION MATRIX: "))
print(cm_NB$table)
print(paste("KNN ACCURACY: ", cm$overall[1]))
print(paste("NAIVE BAYES ACCURACY: ", cm_NB$overall[1]))
print(paste("KNN JACCARD: ", jaccard_knn))
print(paste("NAIVE BAYES JACCARD: ", jaccard_NB))
print(paste("KNN JOUDEN: ", jouden_knn))
print(paste("NAIVE BAYES JOUDEN: ", jouden_NB))
print(paste("KNN MARKEDNESS: ", markedness_knn))
print(paste("NAIVE BAYES MARKEDNESS: ", markedness_NB))


