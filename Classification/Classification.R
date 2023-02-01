HDDataset <- read.csv("C:\\Users\\ferra\\Desktop\\Progetto\\Classification\\heart.csv")

HDDataset <- HDDataset[,c(1:13)]

summary(HDDataset)

set.seed(111)
index <- sample(2, nrow(HDDataset), replace = TRUE, prob = c(0.8, 0.2))

HDTrainingDataset <- HDDataset[index==1,]
HDTestingDataset <- HDDataset[index==2,]

HDTrainingDataset
HDTestingDataset

############################ MAYBE ############################

# Since PCA is based only on indipendent variables we remove the 14th variable from the dataset
heartDiseaseSet.pca <- prcomp(heartDiseaseSet[,c(1:13)], center=TRUE, scale. = TRUE)

summary(heartDiseaseSet.pca)
