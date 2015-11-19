# Week 3 Practical Machine Learning
# Lecture 2: Bagging

# load packages
library(ElemStatLearn)
library(caret)
library(ggplot2)

# load data
data(ozone, package="ElemStatLearn")

# order the ozone data by the ozone variable
ozone <- ozone[order(ozone$ozone),]
head(ozone)

# doing a bagged loess analysis
# create empty matrix with 10 rows, since we'll be doing 10 replicates of 
# the loess curve fitting, and predicting temperatures of 155 new ozone values
ll <- matrix(NA, nrow=10, ncol=155)

set.seed(1)
for (i in 1:10) {
  
  # resample the rows. dim(ozone)[1] just gives the number of rows in the ozone dataframe
  ss <- sample(1:dim(ozone)[1], replace=TRUE)
  ozone0 <- ozone[ss,]
  
  # order the new ozone variables by the ozone variable values
  ozone0 <- ozone0[order(ozone0$ozone),]

    # fit the loess curve to the randomised data set
  # we model temperature as a function of ozone
  loess0 <- loess(temperature ~ ozone, data=ozone0, span=0.2)
  
  # we now use the fitted loess curve to predict temperatures for 
  # new ozone values ranging from 1 to 155
  ll[i,] <- predict(loess0, newdata=data.frame(ozone=1:155))
  
}
  
# we plot each of the loess curves as well as an average of all the curve
# plot the actual true, observed values against each other
plot(ozone$ozone, ozone$temperature, pch=19, cex=0.5)

# plot each of the predictions as a line in turn
for(i in 1:10){
  lines(1:155, ll[i,], col="grey", lwd=2)
}

# plot the averaged predictions as a line
lines(1:155, apply(ll, 2, mean), col="red", lwd=2)

# caret has a bag function for custom bagging
# let's a do a bagged tree prediction of temperature from the ozone variable
# we identify the predictors and the response variable
predictors = data.frame(ozone=ozone$ozone)
temperature = ozone$temperature

# we set up the tree bagging, feeding in the predictors and outcome variable, the
# number of resamples/aggregations we want to do, and the method we'd like to use to predict
treebag <- bag(predictors, temperature, B = 10, 
               bagControl = bagControl(fit=ctreeBag$fit, 
                                       predict = ctreeBag$pred,
                                       aggregate = ctreeBag$aggregate))

# plot the custom bagging results
# plot just the actual values from the original data set
plot(ozone$ozone, temperature, col="lightgrey", pch=19)

# plot one of the fits from the bagging ensemble of predictions
points(ozone$ozone, predict(treebag$fits[[1]]$fit, predictors), pch=19, col="red")

# plot the aggregates predictions for comparison
points(ozone$ozone, predict(treebag, predictors), pch=19, col="blue")

