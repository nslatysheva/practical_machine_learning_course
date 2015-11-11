# Week 2 Practical Machine Learning
# Lecture 2: data slicing

# load packages
# as before, do e.g. install.packages("caret") if you haven't used the package before
library(caret)
library(kernlab)

# load the spam dataset
data(spam)

# we will create a list of indices (inTrain) that will cover 75% of the full dataset
# the reason we specify y=spam$type is because this allows us to sample by the percentiles of these values and
# maintain an even distribution in both the training and testing indices. Type ?createDataPartition for more info
inTrain <- createDataPartition(y=spam$type, p=0.75, list=FALSE)
?createDataPartition

# using the inTrain indices, we designate a training set
# the testing set is just all the indices that aren't in inTrain
training <- spam[inTrain,]
testing <- spam[-inTrain,]

# output the dimensions of the training set
dim(training)

# we may want to create folds of the data for cross validation
# again, we pass the outcome we'd like to split on, the number of folds k
# we can have it return the training set, or the testing set
set.seed(32323)
folds <- createFolds(y=spam$type, k=10, list=TRUE, returnTrain=TRUE)

# check the length of each fold using the sapply function. See ?sapply
sapply(folds, length)
folds[[1]][1:10]

# we can also do resampling with replacement (e.g. bootstrapping)
folds <- createResample(y=spam$type, times=10, list=TRUE)
sapply(folds, length)

# we can get the same data point multiple times in each sample, like the observation "3" here
folds[[1]][1:10]

# one other way to do data splicing is creating time slices
# slices have about 20 samples in them, and we want to create about the next 10 values (horizon)
tme <- 1:1000
folds <- createTimeSlices(y=tme, initialWindow = 20, horizon = 10)
names(folds)

# the train and test sets are adjacent in time
# so that you could use the models from each training set to 
# predict the events happening immediately in the "future"
folds$train[[1]]
folds$test[[1]]
