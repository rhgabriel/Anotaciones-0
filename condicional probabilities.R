library (tidyverse)
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))

# What is the probability that a test is positive?
mean(test==1)

# What is the probability that an individual has the disease if the test is negative?
mean(test==0 & disease==1)

# What is the probability that you have the disease if the test is positive?

mean(disease[test==1] == 1)

#Compare the prevalence of disease in people who test positive to the overall prevalence of disease.
#If a patient's test is positive, by how many times does that increase their risk of having the disease?
#First calculate the probability of having the disease given a positive test, 
#then divide by the probability of having the disease.

mean(disease[test==1] == 1)/ mean(disease==1)
------------------------------------------------------------------------------------------------
#We are now going to write code to compute conditional probabilities for being male in the heights dataset.
#Round the heights to the closest inch. Plot the estimated conditional probability 
  
library(dslabs)
data("heights")

heights %>% 
  mutate(height = round(height)) %>%
  group_by(height) %>%
  summarize(p = mean(sex == "Male")) %>%
  qplot(height, p, data =.)
  
view(heights)

#In the plot we just made in Q6 we see high variability for low values of height.
#This is because we have few data points. This time use the quantile and the cut() function 
#to assure each group has the same number of points. Note that for any numeric vector x,
#you can create groups based on quantiles like this: cut(x, quantile(x, seq(0, 1, 0.1)), include.lowest = TRUE).   

ps <- seq(0, 1, 0.1)
heights %>% 
  mutate(g = cut(height, quantile(height, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(p = mean(sex == "Male"), height = mean(height)) %>%
  qplot(height, p, data =.) 
  
#You can generate data from a bivariate normal distrubution using the MASS package using the following code: 
install.packages("MASS")
library(MASS)  
Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))  
  
And you can make a quick plot using plot(dat).

#Using an approach similar to that used in the previous exercise,
#let's estimate the conditional expectations and make a plot.
#Part of the code has again been provided for you: 
  
ps <- seq(0, 1, 0.1)
dat %>% 
  mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(y = mean(y), x = mean(x)) %>%
  qplot(x, y, data =.)
