# Week 2 Practical Machine Learning
# Lecture 8: predicting with regression

# load packages
library(caret); library(ggplot2); data(faithful); set.seed(333)

# create train and test sets
inTrain <- createDataPartition(y=faithful$waiting, p=0.50, list=FALSE)
trainFaith <- faithful[inTrain,]
testFaith <- faithful[-inTrain,]

head(trainFaith)

# make a plot of this simple data set
plot(trainFaith$waiting, trainFaith$eruptions, pch=19, col="blue", xlab="Waiting", ylab="Duration")

# or, same thing in ggplot2
# this might look a lot more convoluted but the ggplot system is so nice and useful that
# it's worth learning it on basic examples even if it seems weirdly complicated now
ggplot(trainFaith, aes(x=trainFaith$waiting, y=trainFaith$eruptions)) + geom_point(size=4, color="blue") + 
  xlab("Waiting") + ylab("Duration")

# the relationship between the variables looks roughly linear
# let's fit a line through it
# the "Estimate" column have the estimated slope and intercept coefficient values
lm1 <- lm(eruptions ~ waiting, data=trainFaith)
summary(lm1)

# plot the fitted line
plot(trainFaith$waiting, trainFaith$eruptions, pch=19, col="blue", xlab="Waiting", ylab="Duration")
lines(trainFaith$waiting, lm1$fitted, lwd=3)

# to predict a new y value, we just multiply an x value by the slope and add the intercept
coef(lm1)[1] + coef(lm1)[2]*80

# we can predict using the lm object, don't always need to extract the coefficients
# get the prediction for a new value of 80
newdata <- data.frame(waiting=80)
predict(lm1, newdata)

# we want to see how the linear model does on the test set
# the line has a slightly worse fit on the test set, which is to be expected, but it
# still does a really good job
par(mfrow=c(1,2))
plot(trainFaith$waiting, trainFaith$eruptions, pch=19, col="blue", xlab="Waiting", ylab="Duration")
lines(trainFaith$waiting, lm1$fitted, lwd=3)
plot(testFaith$waiting, testFaith$eruptions, pch=19, col="blue", xlab="Waiting", ylab="Duration")
lines(testFaith$waiting, predict(lm1, newdata=testFaith), lwd=3)

# we can calculate the RMSE on the training and test sets
# we observe a slightly larger RMSE on the test set
# train error:
sqrt(sum((lm1$fitted-trainFaith$eruptions)^2))
# test error. A more accurate estimate of the error you would get on a new data set:
sqrt(sum((predict(lm1, newdata=testFaith)-testFaith$eruptions)^2))

# prediction intervals - where we expect most predicted values to land
# the range of possible predictions you're likely to get out
pred1 <- predict(lm1, newdata=testFaith, interval="prediction")
ord <- order(testFaith$waiting)
plot(testFaith$waiting, testFaith$eruptions, pch=19, col="blue")
matlines(testFaith$waiting[ord], pred1[ord,], type="l", col=c(1,2,2), lty=c(1,1,1), lwd=3)

# the same process can be done with the caret package
modFit <- train(eruptions ~ waiting, data=trainFaith, method="lm")
summary(modFit$finalModel)
