install.packages("rpart")

#Create a simple dataset where the outcome grows 0.75 units
#on average for every increase in a predictor, using this code:

library(rpart)
n <- 1000
sigma <- 0.25
set.seed(1, sample.kind = "Rounding")
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)

#Which code correctly uses rpart() to fit a regression tree and saves the result to fit?

fit <- rpart(y ~ ., data = dat)

#Which of the following plots has the same tree shape obtained in Q1?
plot(fit)
text(fit)

#Below is most of the code to make a scatter plot of y versus x
#along with the predicted values based on the fit.

library(tidyverse)

dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col=2)

#Now run Random Forests instead of a regression tree using randomForest()
#from the randomForest package, and remake the scatterplot with the prediction line.
#Part of the code is provided for you below.

library(randomForest)
fit <-  randomForest(y ~ x, data = dat) 
  dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")
  
#Use the plot() function to see if the Random Forest from Q4 has converged or if we need more trees.
  
  plot (fit)
  
#It seems that the default values for the Random Forest result in an estimate
#that is too flexible (unsmooth). Re-run the Random Forest but this time with a
#node size of 50 and a maximum of 25 nodes. Remake the plot.
  
  library(randomForest)
  fit <- randomForest(y ~ x, data = dat, nodesize = 50, maxnodes = 25)
    dat %>% 
    mutate(y_hat = predict(fit)) %>% 
    ggplot() +
    geom_point(aes(x, y)) +
    geom_step(aes(x, y_hat), col = "red")