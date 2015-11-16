# Week 3 Practical Machine Learning
# Lecture 3: Random forests

# load packages
library(ggplot2)
library(caret)

# load dataset
data(iris); names(iris)

# do the train test split
inTrain <- createDataPartition(y=iris$Species, p=0.70, list=FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]

# output the dimensions of the sets
dim(training); dim(testing)

# fit the rf model
# prox=TRUE produces some extra information
modFit <- train(Species ~ ., method="rf", prox=TRUE, data=training)
modFit

# we can look at specific trees using the getTree function
# each row is a particular split
getTree(modFit$finalModel, k=2)

# we can use the class center (centers of predicted values) information to make some nice predictive plots
irisP <- classCenter(training[,c(3,4)], training$Species, modFit$finalModel$prox)
irisP <- as.data.frame(irisP); irisP$Species <- rownames(irisP)

# plot the center and points
p <- qplot(Petal.Width, Petal.Length, col=Species, data=training)
p + geom_point(aes(x=Petal.Width, y=Petal.Length, col=Species), size=5, shape=4, data=irisP)

# now we predict new values using our model
pred <- predict(modFit, testing); testing$predRight <- pred==testing$Species

# generate confusion table
table(pred, testing$Species)

# we can have a look to see which two points we misclassified
qplot(Petal.Width, Petal.Length, colour=predRight, data=testing, main="newdata Predictions")
