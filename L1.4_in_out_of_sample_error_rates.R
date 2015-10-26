# Week 1 Practical Machine Learning
# Lecture 4: demonstration of in and out of sample errors

# load kernlab library and get spam dataset
library(kernlab) # roughly the same as the command "require(kernlab)", this just loads the library
data(spam)

# this just sets a seed, meaning that "random" numbers become completely predefined (though still "random-like")
# this allows perfect reproducibility, because otherwise everyone in a teaching course would sample different numbers
set.seed(333)

# dim(spam) gives the dimension of the dataset (number of rows and columns), 
# and dim(spam)[1] takes the first value of that (number of rows), in this case 4601
# so, this command picks 10 numbers at random from between 1 and 4601
smallSpam <- spam[sample(dim(spam)[1], size=10),]

# this command is interesting, multiplying a boolean vector by 1 returns a sequence of 1s and 0s
# adding 1 onto that converts it to a sequence of 1s and 2s
spamLabel <- (smallSpam$type=="spam")*1 + 1
plot(smallSpam$capitalAve, col=spamLabel)

# let's create a classifier of emails based on values of the capitalAve score

rule1 <- function(x) {
  
  # generate a vector of blank (NA) values with same length as some input vector x
  prediction <- rep(NA, length(x))
  
  # classify cases, if above 2.7 or between 2.4-2.45 it's spam, etc.
  prediction[x > 2.7] <- "spam"
  prediction[x < 2.40] <- "nonspam"
  prediction[x>=2.40 & x <= 2.45] <- "spam"
  prediction[x > 2.45 & x <= 2.70] <- "nonspam"
  
  # return (i.e. send output of) the vector of predictions
  return(prediction)
  
  #   # note, I think a more concise way of coding this is simply:
  #   prediction <- ifelse((x > 2.8 | (x>=2.40 & x <= 2.45)), "spam", "nonspam")
  #   return(prediction)
  
}

# tabulates cooccurences of "nonspam" and "spam" across the two vectors
# i.e. we count the number of times the true and predicted values agree
table(rule1(smallSpam$capitalAve), smallSpam$type)  

# an alternative rule doesn't train so tightly to the training set 
# but still captures the general principle
rule2 <- function(x) {
  
  prediction <- rep(NA, length(x))
  prediction[x > 2.8] <- "spam"
  prediction[x <= 2.8] <- "nonspam"
  return(prediction)
  
  #   # note, the more concise way of coding this up is even simpler now:
  #   prediction <- ifelse(x > 2.8, "spam", "nonspam")
  #   return(prediction)
  #   # or if you like one liners:
  #   return(ifelse(x > 2.8, "spam", "nonspam"))  
}

table(rule2(smallSpam$capitalAve), smallSpam$type)

# now we apply both rules to the entire spam dataset
# calculate the confusion matrix (recall that off diagonals are the errors) and also the accuracy
# the mean command just calculates the percentage of TRUE values in a vector
table(rule1(spam$capitalAve), spam$type)
mean(rule1(spam$capitalAve)==spam$type)

# same for rule 2
table(rule2(spam$capitalAve), spam$type)
mean(rule2(spam$capitalAve)==spam$type)

# we can also look the number of times the rules made the right prediction
# the sum command just counts the number of TRUE values in a vector
# we get a better result from the simplified rule
sum(rule1(spam$capitalAve)==spam$type)
sum(rule2(spam$capitalAve)==spam$type)