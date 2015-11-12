# Week 2 Practical Machine Learning
# Lecture 7: preprocessing with principal components analysis

# load packages
library(caret); library(kernlab); data(spam)

inTrain <- createDataPartition(y=spam$type, p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]

# leave out the outcome column
# take the correlation between all variables, and take the absolute value
M <- abs(cor(training[,-58]))

# the diagonals are normally 1, set that to 0
diag(M) <- 0

# which of the variables have a correlation over 0.8?
# num415 and num857 tend to appear together, maybe due to phone numbers
which(M > 0.8, arr.ind = T)

# it's these two columns
names(spam)[c(34,32)]

# plot these two variables against each other, and as expected,
# these are incredibly correlated, might not be useful to include both of them in the same model
plot(spam[,34], spam[,32])

# we're trying to find combinations of variables to try to explain the variability
# just as an example, try adding and subtracting
# adding the variables together capture more information, the points vary more along that axis
X <- 0.71*training$num415 + 0.71*training$num857
Y <- 0.71*training$num415 - 0.71*training$num857
plot(X,Y)

# do the PCA 
smallSpam <- spam[,c(34,32)]
prComp <- prcomp(smallSpam)
plot(prComp$x[,1], prComp$x[,2])

# you can also take a look at the rotation matrix, or how it's summing up the two variables
prComp$rotation

# typeColor is just 2 where spam, 1 where not spam
typeColor <- ((spam$type=="spam")*1 + 1)
# calculate the principal components on entire dataset
prComp <- prcomp(log10(spam[,-58]+1))
# along PC1 there is a bit separation between the spam and ham messages
plot(prComp$x[,1], prComp$x[,2], col=typeColor, xlab="PC1", ylab="PC2")

# PCA can be done with caret using preprocess function
preProc <- preProcess(log10(spam[,-58]+1), method="pca", pcaComp=2)
spamPC <- predict(preProc, log10(spam[,-58]+1))
# observe the separation between ham and spam
plot(spamPC[,1], spamPC[,2], col=typeColor)

# do this on training
preProc <- preProcess(log10(training[,-58]+1), method="pca", pcaComp=2)
trainPC <- predict(preProc, log10(training[,-58]+1))
modelFit <- train(training$type ~ ., method="glm", data=trainPC)

# apply components to prediction
testPC <- predict(preProc, log10(testing[,58]+1))
confusionMatrix(testing$type, predict(modelFit, testPC))

# the fastest way to do this:
modelFit <- train(training$type ~ . , method="glm", preProcess="pca", data=training)
confusionMatrix(testing$type, predict(modelFit, testing))
