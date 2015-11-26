# Week 4 Practical Machine Learning
# Lecture 2: Combining predictors

# load packages
library(ISLR)
library(caret)
library(ggplot2)

# get data, subset out the outcome
data(Wage)
Wage <- subset(Wage, select=-c(logwage))

# do the validation, train, test split
inBuild <- createDataPartition(y=Wage$wage, p=0.70, list=FALSE)
buildData <- Wage[inBuild,]
validation <- Wage[-inBuild,]

inTrain <- createDataPartition(y=buildData$wage, p=0.70, list=FALSE)
training <- buildData[inTrain,]
testing <- buildData[-inTrain,]

# output the dimensions of the sets
dim(training); dim(testing); dim(validation)

# build two different models
mod1 <- train(wage ~., method="glm", data=training)
mod2 <- train(wage ~., method="rf", data=training, trControl= trainControl(method="cv"), number=3)

# predict on the testing set
pred1 <- predict(mod1, testing)
pred2 <- predict(mod2, testing)

# plot predictions against each other
qplot(pred1, pred2, colour=wage, data=testing)

# fit a model that combines predictors
predDF <- data.frame(pred1, pred2, wage=testing$wage)
combModFit <- train(wage ~., method="gam", data=predDF)
combPred <- predict(combModFit, predDF)

# compare testing errors on the 2 models and the combined model
sqrt(sum((pred1-testing$wage)^2))
sqrt(sum((pred2-testing$wage)^2))
sqrt(sum((combPred-testing$wage)^2))

# predict on validation data set
pred1V <- predict(mod1, validation)
pred2V <- predict(mod2, validation)
combPredV <- predict(combModFit, predVDF)

# evaluate predictions on validation set
sqrt(sum((pred1-validation$wage)^2))
sqrt(sum((pred2-validation$wage)^2))
sqrt(sum((combPred-validation$wage)^2))



