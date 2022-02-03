library(tidyverse)
library(caret)

set.seed(1, sample.kind="Rounding") 
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))


set.seed(1, sample.kind="Rounding")
ans<-replicate (n,{test_index<- createDataPartition(dat$y, times=1, p=0.5, list = FALSE) 
    train_set<- dat [-test_index,]
    test_set<- dat[test_index,]
  fit<- lm(y~x, data= train_set)
  y_hat<- predict(fit, test_set)
  mse<-mean((y_hat-test_set$y)^2)
  rmse<-sqrt(mse)
})
#ans nos está devolviendo el rmse
mean(ans)
sd(ans)

# Write a function that takes a size n, then
#  (1) builds a dataset using the code provided in Q1 but with n observations
#      instead of 100 and without the set.seed(1),
#  (2) runs the replicate loop that you wrote to answer Q1, which builds 100
#      linear models and returns a vector of RMSEs, and
#  (3) calculates the mean and standard deviation.

  n <- c(100, 500, 1000, 5000, 10000)
q2<- function(n) {Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

ans2<-replicate (100,{test_index<- createDataPartition(dat$y, times=1, p=0.5, list = FALSE) 
train_set<- dat [-test_index,]
test_set<- dat[test_index,]
fit<- lm(y~x, data= train_set)
y_hat<- predict(fit, test_set)
mse<-mean((y_hat-test_set$y)^2)
rmse<-sqrt(mse)
})
mean(ans2)
sd(ans2)
}
set.seed(1, sample.kind="Rounding")
resq2<-map(n,q2)
#la respuesta lo hizo así
set.seed(1, sample.kind="Rounding") # if R 3.6 or later
n <- c(100, 500, 1000, 5000, 10000)
res <- sapply(n, function(n){
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  rmse <- replicate(100, {
    test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
    train_set <- dat %>% slice(-test_index)
    test_set <- dat %>% slice(test_index)
    fit <- lm(y ~ x, data = train_set)
    y_hat <- predict(fit, newdata = test_set)
    sqrt(mean((y_hat-test_set$y)^2))
  })
  c(avg = mean(rmse), sd = sd(rmse))
})
res
--------------------------------------------
  set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
set.seed(1, sample.kind="Rounding")
ans4<-replicate (n,{test_index<- createDataPartition(dat$y, times=1, p=0.5, list = FALSE) 
train_set<- dat [-test_index,]
test_set<- dat[test_index,]
fit<- lm(y~x, data= train_set)
y_hat<- predict(fit, test_set)
mse<-mean((y_hat-test_set$y)^2)
rmse<-sqrt(mse)
})
mean(ans4)
sd(ans4)
---------------------------------------
#Note that y is correlated with both x_1 and x_2
  #but the two predictors are independent of each other, as seen by cor(dat).
  
  set.seed(1, sample.kind="Rounding")
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)

fit <- lm(y ~ x_1, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))

fit <- lm(y ~ x_2, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))

fit <- lm(y ~ x_1 + x_2, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))
--------------------------------------------------
 #Repeat the exercise from Q6 but now create an example in which x_1 and x_2 are highly correlated.
  
   set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)

fit <- lm(y ~ x_1, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))

fit <- lm(y ~ x_2, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))

fit <- lm(y ~ x_1 + x_2, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))

#Adding extra predictors can improve RMSE substantially, 
#but not when the added predictors are highly correlated with other predictors. 
  