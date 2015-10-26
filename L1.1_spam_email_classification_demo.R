# Week 1 Practical Machine Learning
# Lecture 2: What is prediction?

# if you have not used the kernlab package before, do install.packages("kernlab")
# load the library and the spam dataset
require(kernlab)
data(spam)

# take a look at the top of the spam dataset
head(spam)

# looking at frequency of the word "your" in spam and ham emails in dataset
# the spam$your[spam$type=="nonspam"] command is doing the main work here
# it is fetching all of the values of "your" (which is the frequency of the word "your" in each email) if the
# email is a nonspam email. We draw a histogram of these values and color the line blue
plot(density(spam$your[spam$type=="nonspam"]), col="blue", main="", xlab="Frequency of 'your'")

# then, on the same plot, we draw another histogram curve ("lines") for the values of "your" for spam emails
lines(density(spam$your[spam$type=="spam"]),col="red")

# we create a threshold based on word frequency
abline(v=0.5,col="black")

# note, a nicer and more popular way of drawing plots in R is using the ggplot package, which would look something like this:
require(ggplot2) # or if you've never used it before, run install.packages("ggplot2") first
qplot(your, data=spam, fill=type, geom="density")

# assign prediction, email is predicted "spam" if frequency of "your" is over 0.5
prediction <- ifelse(spam$your > 0.5, "spam", "nonspam")

# output percentage of dataset that was actually nonspam and was predicted nonspam, 
# or was actually nonspam but was predicted as spam, etc.
table(prediction, spam$type)/length(spam$type)



