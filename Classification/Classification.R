install.packages("caTools")
install.packages("class")
install.packages("outliers")
library(caTools)
library(class)
library(outliers)


HD_Dataset <- read.csv("C:\\Users\\ferra\\Desktop\\Progetto\\Classification\\heart.csv")

# We start with PCA feature extraction and we choose only the features with a proportion of variance > 0.05.
heartDiseaseSet.pca <- prcomp(HD_Dataset[,c(1:13)], center=TRUE, scale. = TRUE)

# Then we create a "cleaned dataset" containing only the 9 "best" features + the output.
HD_Dataset_Cleaned <- HD_Dataset[c('age','sex','cp','trtbps','chol','fbs','restecg','thalachh','exng','output')]

# We used the z_score and set the maximum value as 3. 
# In addition, we removed the rows having at least one z-score with an absolute value greater than 3.
z_scores <- as.data.frame(sapply(HD_Dataset_Cleaned, function(data) (abs(data-mean(data))/sd(data))))
no_outliers <- z_scores[!rowSums(z_scores>3), ]

# Then we normalized the dataset so that the output remains unbiased.
normalize <- function(x){return ((x-min(x))/(max(x)-min(x)))}
HD_Dataset_Normalized <- as.data.frame(lapply(no_outliers[,1:10], normalize))

# We split the dataset into training set and testing set.
split <- sample.split(no_outliers, SplitRatio = 0.7)
train_HD <- subset(no_outliers, split == "TRUE")
test_HD <- subset(no_outliers, split == "FALSE")

train_Scale_HD <- scale(train_HD[,1:10])
test_Scale_HD <- scale(test_HD[,1:10])

# In order to find a the suitable value of K we just count the number of rows of the training set 
# and choose K as the square root of the number of rows.
NROW(train_Scale_HD)
#Result = 207 → sqrt(207)=14.38 → K = 14

# Then we set our K-NN classifier with K = 14
KNN_Classifier <- knn(train = train_Scale_HD,
                      test = test_Scale_HD,
                      cl = train_HD$output,
                      k=14)

# We used a confusion matrix just to check our prediction.
conf_matrix <- table(test_HD$output, KNN_Classifier)
conf_matrix

# Finally, we have calculated the misclassification error and the accuracy.
misClassError <- mean(KNN_Classifier != test_HD$output)
print(paste('Accuracy =', 1-misClassError))
