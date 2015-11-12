# Week 2 Practical Machine Learning
# Lecture 5: preprocessing

# load packages
library(caret)
library(kernlab)
data(spam)

# do the train test split
inTrain <- createDataPartition(y=spam$type, p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]

# how many capitals do we see in a row? i.e. capital run length
# this distribution is very skewed, may want to preprocess
# so ML algorithms don't get tricked by the fact that it is highly variable
hist(training$capitalAve, main="", xlab="ave. capital run length")
mean(training$capitalAve)
sd(training$capitalAve)

# standardise the variable
trainCapAve <- training$capitalAve
trainCapAveS <- (trainCapAve - mean(trainCapAve))/sd(trainCapAve)

#summarise the standardised variable
mean(trainCapAveS)
sd(trainCapAveS)

# to standardise the test set, we use the same means and standard deviations as from the training set!
testCapAve <- testing$capitalAve
testCapAveS <- (testCapAve - mean(trainCapAve))/sd(trainCapAve)

# summarise the standardised testing data
mean(trainCapAveS)
sd(trainCapAveS)

# we can use the preprocess function to do a lot of standardisation for you
# instead of doing it by hand
# this command centers and scales every variable
preObj <- preProcess(training[,-58], method=c("center", "scale"))

# we can use the object created by preprocessing the training set to apply the same set of
# operations on the test set if we use the predict function
testCapAveS <- predict(preObj, testing[,-58])$capitalAve
# the results won't exactly have a mean of 0 and a s.d. of 1 but it'll be close
mean(testCapAveS)
sd(testCapAveS)

# we can also pass the preprocess command directly to the train function
set.seed(32343)
modelFit <- train(type ~., data=training, preProcess=c("center", "scale"), method="glm")
modelFit

# we can do other kinds of transformations in addition to centering and scaling
# can also transform data into certain forms
# the Box-Cox transforms take continuous data and try to make them look like normal data
# they estimate certain parameters using maximum likelihood
preObj <- preProcess(training[,-58], method=c("BoxCox"))
trainCapAveS <- predict(preObj, testing[,-58])$capitalAve

# examine transformed value distribution and QQ plot
par(mfrow=c(1,2))
hist(trainCapAveS)
qqnorm(trainCapAveS)

# data imputation - filling in missing data
# we can use k nearest neighbours to find the most similar data points to missing points
# then average the values of the neighbours to fill in the blanks
set.seed(13343)

# make some values be missing (NA, or not available)
# to randomly choose which values will be missing, we use a binomial distribution with 
# probability of "success" p = 0.05. We sample from this distribution n times, where n is the 
# number of rows in the training set, given by dim(training)[1]
# we then select where this distribution gives a value of TRUE, or "success", and use those 
# indices to subset the original dataset to just the set we want to artificially label NA
training$capAve <- training$capitalAve
selectNA <- rbinom(dim(training)[1], size=1, prob=0.05)==1
training$capitalAve[selectNA] <- NA

# impute and standardize
preObj <- preProcess(training[,-58], method="knnImpute")
capAve <- predict(preObj, training[,-58])$capAve

# standardize the values
capAveTruth <- training$capitalAve
capAveTruth <- (capAveTruth-mean(capAveTruth))/sd(capAveTruth)

# look at differences in truth vs not
quantile(capAve - capAveTruth, na.rm=TRUE)
quantile((capAve - capAveTruth))[selectNA]
quantile((capAve-capAveTruth)[!selectNA])
