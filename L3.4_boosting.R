# Week 3 Practical Machine Learning
# Lecture 4: boosting

# load packages
library(ISLR)
library(ggplot2)
library(caret)

# do the train test split
Wage <- subset(Wage, select=-c(logwage))
inTrain <- createDataPartition(y=Wage$wage, p=0.70, list=FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]

# we can then model wage as a function of all other variables using gbm, 
# which does boosting with trees
modFit <- train(wage ~., method="gbm", data=training, verbose=FALSE)
print(modFit)

# plot predicted results from testing set versus real values
qplot(predict(modFit, testing), wage, data=testing)
