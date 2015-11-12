# Week 2 Practical Machine Learning
# Lecture 9: predicting with multivariate regression

# load packages
library(caret); library(ggplot2); library(caret)

# attach data and remove the thing we're trying to predict for now
data(Wage)
Wage <- subset(Wage, select=-c(logwage))
summary(Wage)

# create train and test sets
inTrain <- createDataPartition(y=Wage$wage, p=0.7, list=FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]

# make a feature plot to show how the variables are related to each other
featurePlot(x=training[,c("age", "education", "jobclass")],
            y=training$wage,
            plot="pairs")

# quick plot of wage against age
qplot(age, wage, colour=jobclass, data=training)

# colour by education
qplot(age, wage, colour=education, data=training)

# fit a multivariate linear model
modFit <- train(wage ~ age + jobclass + education,
                method="lm", 
                data=training)

finMod <- modFit$finalModel
print(modFit)

# look at some diagnostic plots
# plot residuals over fitted values, expect a straight horizontal line at 0
plot(finMod, 1, pch=19, cex=0.5, col="#00000010")

# we can also colour the points on the diagnostics plots by some other variable
# looks like the outliers could be partially explained by race
qplot(finMod$fitted.values, finMod$residuals, colour=race, data=training)

# another thing we can do is plot the fitted residuals over the index
# whenever you can see a trend with respect to row numbers, suggests you're missing some sort of variable
plot(finMod$residuals, pch=19)

# we could also plot the predicted wage over the actual wage in the test set
# this is more of a post mortem, the information you find here can't be used to go back and make a better model
pred <- predict(modFit, testing)
qplot(wage, pred, colour=year, data=testing)

# if we wanted to predict using all covariates:
modFitAll <- train(wage ~ ., data=training, method = "lm")
pred <- predict(modFitAll, testing)
qplot(wage, pred, colour=year, data=testing)

