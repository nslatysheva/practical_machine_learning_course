# Week 4 Practical Machine Learning
# Lecture 3: Forecasting

# load packages
library(quantmod)
library(ggplot2)
library(caret)

# get GOOG stock data
from.dat <- as.Date("01/01/08", format="%m/%d/%y")
to.dat <- as.Date("12/31/13", format="%m/%d/%y")
getSymbols("GOOG", src="google", from= from.dat, to= to.dat)

# check out the stock data
head(GOOG)

# summarise monthly data and store as time series
# this appears to be outdated code, no longer functional
mGoog <- to.monthly(GOOG)
googOpen <- Op(mGoog)
ts1 <- ts(googOpen, frequency = 12)
plot(ts1, xlab="Years+1", ylab="GOOG")

# time series data decomposition
plot(decompose(ts1), xlab="Years+1")

# do the train test split
ts1Train <- window(ts1, start=1, end=5)
ts1Test <- window(ts1, start=5, end=(7-0.01))
ts1Train
 
# plot a simple moving average
plot(ts1Train)
lines(ma(ts1Train, order=3), col="red")

# plot exponential smoothing
ets1 <- ets(Ts1Train, model="MMM")
fcast <- forecast(ets1)
plot(fcast)
lines(ts1Test, col="red")

# get the accuracy out
accuracy(fcast, ts1Test)