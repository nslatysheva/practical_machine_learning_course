# Week 2 Practical Machine Learning
# Lecture 1: caret package

# load packages
# as before, do e.g. install.packages("caret") if you haven't used the package before
# also, if you've recently upgraded your version of R you'll need to reinstall packages
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

# fit a general linear model (glm) to the spam dataset
# set the seed
set.seed(32343)

# here, we are creating a glm that models the type of the email (spam or not spam) on the basis of every other variable in the dataset
# the notation for "use every possible predictor" is the full stop, "."
# so "type ~ ." means, model the type as a function of every variable in the dataset
modelFit <- train(type ~ ., data=training, method = "glm")

# output a summary of the model
modelFit

# the actual final model
modelFit$finalModel

# we can now use this model to do the prediction
# in this case we are applying the trained model to the testing data
predictions <- predict(modelFit, newdata=testing)
predictions

# one way to see how well the model did on the test data is by showing the confusion matrix
# this shows which categories were misclassified as what, as well as some statistics that
# summarise the performance (accuracy, sensitivity, specificity, etc.)
confusionMatrix(predictions, testing$type)
