# Week 4 Practical Machine Learning
# Lecture 1: Regularised regression

# load packages
library(ElemStatLearn)

# load data
data(prostate)
str(prostate)

# form a small dataset and try to fit a linear model
# we get NAs because we have more predictors than data points
small = prostate[1:5,]
lm(lpsa ~., data=small)
