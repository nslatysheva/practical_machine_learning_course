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

# we can use a featurePlot to see all variables at the same time
featurePlot(x=training[,c("age", "education", "jobclass")],
            y=training$wage,
            plot="pairs")

# we can also do a qplot, or quick plot, from the ggplot package to plot all variables against each other
# it appears that wage tends to increase with age a bit
# and we find that there is a group that is making much more than everyone else
qplot(age, wage, data=training)

# we can colour the points by the type of job, industrial versus information, 
# to see if this could help explain this trend
# we can see that most of the individuals in the upper chunk are doing information jobs
# gives you a way to see what variables could be important in the model, because 
# they break up the data into categories that seem important or informative
qplot(age, wage, colour=jobclass, data=training)

# we can add regression smoothers to the different classes of education to investigate if
# there is a different relationship between wage and age according to education level
qq <- qplot(age, wage, colour=education, data=training)
qq + geom_smooth(method="lm", formula=y~x)

# another thing that might be useful is to break up the wage into different categories because
# it is becoming clear that there are these two distinct groups
# here we break up the data into 3 groups based on quantiles
cutWage <- cut2(training$eage, g=3)

# we can now draw boxplot
p1 <- qplot(cutWage, age, data=training, fill=cutWage, geom=c("boxplot"))
p1

# we might want to add points on top of the boxplot
p2 <- qplot(cutWage, age, data=training, fill=cutWage, geom=c("boxplot", "jitter"))
grid.arrange(p1, p2, ncol=2)

# another thing that's really useful is to look at different types of tables of the data
# e.g. this shows that there are more lower wage jobs that are industrial
t1 <- table(cutWage, training$jobclass)
t1

# a proportional table shows the same data but showing percentages instead
prop.table(t1,1)

# density plots (basically smoothed histograms) are also useful to look at
qplot(wage, colour=education, data=training, geom="density")
