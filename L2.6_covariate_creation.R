# Week 2 Practical Machine Learning
# Lecture 4: plotting predictors

# load packages
# as before, do e.g. install.packages("ISLR") if you haven't used the package before
library(ISLR)
library(caret)
library(ggplot2)
library(Hmisc) # for cut2 method

# load the wage dataset and look at a summary of the dataset to
# see all the variables and their summary statistics
# we can see from the values it's a bit of a weird dataset, they're all males from the middle atlantic
data(Wage)
summary(Wage)

# do the train test split
inTrain <- createDataPartition(y=Wage$wage, p=0.70, list=FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]

# output the dimensions of the training set
dim(training)

# create a table of the job class variable
table(training$jobclass)

# generate dummy variables
# creates new columns with binary labels of factors that were multiclass before
dummies <- dummyVars(wage ~ jobclass, data=training)
head(predict(dummies, newdata=training))

# removing zero covariates (some factors have no variability at all, and probably won't be interesting at all)
nsv <- nearZeroVar(training, saveMetrics=TRUE)
nsv

# instead of lm or glm, sometimes we want to fit curvy lines
# basis functions are one way of doing this
# bs function fits polynomials (to the third degree, in this case)
# that's one way to generate new variables, by allowing more flexibility in how you 
# model those variables (e.g. allowing polynomials)
library(splines)
bsBasis <- bs(training$age, df=3)
bsBasis

# let's plot the polynomials
lm1 <- lm(wage ~ bsBasis, data=training)
plot(training$age, training$wage, pch=19, cex=0.5)
points(training$age, predict(lm1, newdata=training), col="red", pch=19, cex=0.5)
