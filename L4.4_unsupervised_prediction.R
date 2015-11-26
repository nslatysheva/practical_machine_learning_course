# Week 4 Practical Machine Learning
# Lecture 4: Unsupervised prediction

# load packages
library(ggplot2)
library(caret)

data(iris)
names(iris)

# do the train test split
inTrain <- createDataPartition(y=iris$Species, p=0.70, list=FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]

# output the dimensions of the sets
dim(training); dim(testing)

# cluster with k means
# plot resulting clusters
kMeans1 <- kmeans(subset(training, select=-c(Species)), center=3)
training$clusters <- as.factor(kMeans1$cluster)
qplot(Petal.Width, Petal.Length, colour=clusters, data=training)

# compare clusters to real species labels to gauge fit
table(kMeans1$cluster, training$Species)

# build a tree predictor
modFit <- train(clusters ~., data=subset(training, select=-c(Species)), method="rpart")
table(predict(modFit, training), training$Species)

# apply predictor on test set
testClusterPred <- predict(modFit, testing)
table(testClusterPred, testing$Species)
