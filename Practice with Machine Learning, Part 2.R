library(caret)
library(tidyverse)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]

y <- iris$Species

set.seed(2, sample.kind="Rounding")
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

y_hat <- sample(c("virginica", "versicolor"), length(train), replace = TRUE) %>% factor(levels=levels(test$Species))

mean(y_hat==train$Species) #es logico que de 0.5 porque se está adivinando

#Using only the train iris dataset, for each feature, perform a simple search to find the cutoff
#hat produces the highest accuracy, predicting virginica if greater than the cutoff and versicolor
#therwise. Use the seq function over the range of each feature by intervals of 0.1 for this search.
#hich feature produces the highest accuracy?

foo <- function(x){
  rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
  sapply(rangedValues,function(i){
    y_hat <- ifelse(x>i,'virginica','versicolor')
    mean(y_hat==train$Species)
  })
}
predictions <- apply(train[,-5],2,foo)
sapply(predictions,max)


#For the feature selected in Q8, use the smart cutoff value from the training data to calculate
#overall accuracy in the test data. What is the overall accuracy?

predictions <- foo(train[,3])
rangedValues <- seq(range(train[,3])[1],range(train[,3])[2],by=0.1)
cutoffs <-rangedValues[which(predictions==max(predictions))]

y_hat <- ifelse(test[,3]>cutoffs[1],'virginica','versicolor')
mean(y_hat==test$Species)
------------------------
#Notice that we had an overall accuracy greater than 96% in the training data,
#but the overall accuracy was lower in the test data. This can happen often if we overtrain.
#In fact, it could be the case that a single feature is not the best choice.
#For example, a combination of features might be optimal.
#Using a single feature and optimizing the cutoff as we did on our training data can lead to overfitting.

#Given that we know the test data, we can treat it like we did our training data to see if the same feature
#with a different cutoff will optimize our predictions. Repeat the analysis in Q8
#but this time using the test data instead of the training data. 
  
   foo <- function(x){
    rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
    sapply(rangedValues,function(i){
      y_hat <- ifelse(x>i,'virginica','versicolor')
      mean(y_hat==test$Species)
    })
  }
predictions <- apply(test[,-5],2,foo)
sapply(predictions,max)	
----------------------------------------------------
plot(iris,pch=21,bg=iris$Species)
# Optimize the the cutoffs for Petal.Length and Petal.Width separately in the train
#  dataset by using the seq function with increments of 0.1. Then, report the overall
#  accuracy when applied to the test dataset by creating a rule that predicts virginica
#  if Petal.Length is greater than the length cutoff OR Petal.Width is greater than
#  the width cutoff, and versicolor otherwise.
foofoofoo <- function(x){
  y_hat <- ifelse(test$Petal.Length>4.7 | test$Sepal.Length>6.7,'virginica','versicolor')
  mean(y_hat==test$Species)
}

# What is the overall accuracy for the test data now?
predictions <- apply(test[,-5], 2, foofoofoo)
dataaa <- sapply(predictions, max)
--------------------------------------------
  petalLengthRange <- seq(range(train$Petal.Length)[1],range(train$Petal.Length)[2],by=0.1)
petalWidthRange <- seq(range(train$Petal.Width)[1],range(train$Petal.Width)[2],by=0.1)

length_predictions <- sapply(petalLengthRange,function(i){
  y_hat <- ifelse(train$Petal.Length>i,'virginica','versicolor')
  mean(y_hat==train$Species)
})
length_cutoff <- petalLengthRange[which.max(length_predictions)] # 4.7

width_predictions <- sapply(petalWidthRange,function(i){
  y_hat <- ifelse(train$Petal.Width>i,'virginica','versicolor')
  mean(y_hat==train$Species)
})
width_cutoff <- petalWidthRange[which.max(width_predictions)] # 1.5

y_hat <- ifelse(test$Petal.Length>length_cutoff | test$Petal.Width>width_cutoff,'virginica','versicolor')
mean(y_hat==test$Species)
