install.packages("caTools")
install.packages("class")
install.packages("caret")
install.packages("Amelia")
library(caTools)
library(class)
library(e1071)
library(caret)
library(Amelia)

HD_Dataset <- read.csv("C:\\Users\\ferra\\Desktop\\Progetto\\Classification\\heart.csv")

# We start with PCA feature extraction and we choose only the features with a proportion of variance > 0.05.
heartDiseaseSet.pca <- prcomp(HD_Dataset, center=TRUE, scale. = TRUE)

# Then we normalized the dataset so that the output remains unbiased.
normalize <- function(x){return ((x-min(x))/(max(x)-min(x)))}
HD_Dataset_Normalized <- as.data.frame(lapply(HD_Dataset, normalize))

# We split the dataset into training set and testing set.
split <- sample.split(HD_Dataset_Normalized, SplitRatio = 0.7)
train_HD <- subset(HD_Dataset_Normalized, split == "TRUE")
test_HD <- subset(HD_Dataset_Normalized, split == "FALSE")


# --------------- K-NN ---------------

train_Scale_HD <- scale(train_HD)
test_Scale_HD <- scale(test_HD)

# In order to find a the suitable value of K we just count the number of rows of the training set 
# and choose K as the square root of the number of rows.
NROW(train_Scale_HD)
#Result = 194 → sqrt(194)=13.6 → K=13 V K = 14

# Then we set our K-NN classifier with K = 13
KNN_Classifier_13 <- knn(train = train_Scale_HD,
                      test = test_Scale_HD,
                      cl = train_HD$output,
                      k=13)

# Then we set our K-NN classifier with K = 14
KNN_Classifier_14 <- knn(train = train_Scale_HD,
                         test = test_Scale_HD,
                         cl = train_HD$output,
                         k=14)

# We used a confusion matrix just to check our prediction.
conf_matrix_13 <- table(test_HD$output, KNN_Classifier_13)
conf_matrix_14 <- table(test_HD$output, KNN_Classifier_14)
conf_matrix_13
conf_matrix_14

# Finally, we have calculated the misclassification error and the accuracy.
misClassError_13 <- mean(KNN_Classifier_13 != test_HD$output)
misClassError_14 <- mean(KNN_Classifier_14 != test_HD$output)
print(paste('Accuracy =', 1-misClassError_13))
print(paste('Accuracy =', 1-misClassError_14))

# --------------- FINE K-NN ---------------
# --------------- NAIVE BAYES  ---------------

NB_Class <- naiveBayes(output ~ ., data = train_HD)

y_Predict <-predict(NB_Class, newdata = test_HD)

conf_matrix_NB <- table(test_HD$output, y_Predict)

confusionMatrix(conf_matrix_NB)
