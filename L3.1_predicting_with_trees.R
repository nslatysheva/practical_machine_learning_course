# Week 3 Practical Machine Learning
# Lecture 1: Predicting with trees

# load packages
library(ggplot2)
library(caret)
data(iris)
names(iris)

# do the train test split
inTrain <- createDataPartition(y=iris$Species, p=0.70, list=FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]

# output the dimensions of the sets
dim(training); dim(testing)

# do some plots
qplot(Petal.Width, Sepal.Width, colour=Species, data=training)

# fit the tree model
modFit <- train(Species ~ ., method="rpart", data=training)
print(modFit$finalModel)

# plot the classification tree
# along with the annotating text
plot(modFit$finalModel, uniform=TRUE, main="Classification Tree")
text(modFit$finalModel, use.n=TRUE, all=TRUE, cex=0.8)

# let's plot a prettier tree
library(rattle)
fancyRpartPlot(modFit$finalModel)

# we can predict new values using the predict function, as with other predictive models
# now we predict category labels of new samples
predict(modFit, newdata=testing)
