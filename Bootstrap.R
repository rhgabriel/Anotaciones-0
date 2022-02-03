#The createResample() function can be used to create bootstrap samples.
#For example, we can create the indexes for 10 bootstrap samples for the mnist_27 dataset like this:

library(dslabs)
library(caret)
data(mnist_27)
set.seed(1995, sample.kind="Rounding")
indexes <- createResample(mnist_27$train$y, 10)

#How many times do 3, 4, and 7 appear in the first resampled index?

sum(indexes[[1]] == 3)
sum(indexes[[1]] == 4)
sum(indexes[[1]] == 7)

#We see that some numbers appear more than once and others appear no times.
#This has to be this way for each dataset to be independent.
#Repeat the exercise for all the resampled indexes.
#What is the total number of times that 3 appears in all of the resampled indexes?


sumin3 <- function(i) { sum(indexes [[i]] == 3)
}
sumin3(2)

sum (sumin3(1),sumin3(2),sumin3(3),sumin3(4),sumin3(5),sumin3(6),sumin3(7),sumin3(8),sumin3(9),sumin3(10))

#o también se puede hacer así
x=sapply(indexes, function(ind){
  sum(ind == 3)
})
sum(x)

#A random dataset can be generated with the following code:
  
y <- rnorm(100, 0, 1)

#Estimate the 75th quantile, which we know is qnorm(0.75), with the sample quantile: quantile(y, 0.75).

#Now, set the seed to 1 and perform a Monte Carlo simulation with 10,000 repetitions,
#generating the random dataset and estimating the 75th quantile each time.
#What is the expected value and standard error of the 75th quantile?
  
#Report all answers to at least 3 decimal digits.

quantile(y, 0.75)

set.seed(1, sample.kind = "Rounding")
B<-10000
quantiles<- replicate(B, { y <- rnorm(100, 0, 1) 
quantile(y, 0.75)})

mean(quantiles)

sd(quantiles)

#In practice, we can't run a Monte Carlo simulation. Use the sample:

set.seed(1, sample.kind = "Rounding")
y <- rnorm(100, 0, 1)

#Set the seed to 1 again after generating y and use 10 bootstrap samples to estimate
#the expected value and standard error of the 75th quantile.

set.seed(1, sample.kind="Rounding")
indexes <- createResample(y, 10)

tt = sapply(indexes, function(ind){
  quantile(y[ind], 0.75)
})
mean(tt)
sd(tt)

#Repeat the exercise from Q4 but with 10,000 bootstrap samples instead of 10. Set the seed to 1 first. 
set.seed(1, sample.kind="Rounding")
indexes <- createResample(y, 10000)

tt = sapply(indexes, function(ind){
  quantile(y[ind], 0.75)
})
mean(tt)
sd(tt)

