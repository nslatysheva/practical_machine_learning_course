# Week 3 Practical Machine Learning
# Lecture 5: model based prediction

# load packages
library(ggplot2)
library(caret)

data(iris)
names(iris)

# table of the species counts
table(iris$Species)

# do the train test split
inTrain <- createDataPartition(y=iris$Species, p=0.70, list=FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]

# output the dimensions of the sets
dim(training); dim(testing)

# building up the predictions
# using linear descriminant analysis (LDA)
modlda <- train(Species ~ ., data=training, method="lda")
# using Naive Bayes (NB)
modnb <- train(Species ~ ., data=training, method="nb")

# make predictions using these two models
plda = predict(modlda, testing)
pnb = predict(modnb, testing)

#compare predictions from the two methods with a table
table(plda, pnb)

# let's create a variable to flag up predictions that differ between the two methods
equalPredictions <- plda==pnb

# plot the predictions and label the discordant predictions 
qplot(Petal.Width, Sepal.Width, colour=equalPredictions, data=testing)
