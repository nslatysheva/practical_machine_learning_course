# Week 2 Practical Machine Learning
# Lecture 3: training control

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

# using the inTrain indices, we designate a training set
# the testing set is just all the indices that aren't in inTrain
training <- spam[inTrain,]
testing <- spam[-inTrain,]

# fit the model using the default options
modelFit <- train(type ~., data=training, method="glm")

# check the default arguments of the train method
args(train.default)

# we can tailor the train method in a lot of different ways
# e.g. method option, set the algorithm to random forrests
train(type ~., data=training, method = "rf")

# weights argument can be used if you have a dataset where there is much more of one category than another

# metric = ifelse(is.factor(y), "Accuracy", "RMSE"))
# default metric is accuracy if the variable you're predicting is a factor, 
# otherwise the variable is continuous and the metric is RMSE: 
         
# the trainControl arguments allow a much finer control of the training process
args(trainControl)

# e.g. method="boot", could be adjusted bootstrapping with boot632, cross validation, 
# repeated cross validation, leave one out cross validation
# number of subsamples to take
# number of times to repeat subsampling
# using parallel processing

# it is often useful to set specific seeds (generate pseudo random numbers) for resampling
# for example, fit a model like below, bootstrap samples will be generated according to the random seem
set.seed(1235)
modelFit2 <- train(type ~., data=training, method="glm")
modelFit2

# now try to train another model
set.seed(1235)
modelFit3 <- train(type ~., data=training, method="glm")
modelFit3

# the bootstrap samples and accuracy will be exactly the same as before
# this is useful for sharing your models with other people, ensures complete replication
