#Previously, we used logistic regression to predict sex based on height.
#Now we are going to use knn to do the same. Set the seed to 1,
#then use the caret package to partition the dslabs heights data into
#a training and test set of equal size. Use the sapply() function
#to perform knn with k values of seq(1, 101, 3) and calculate
#F1 scores with the F_meas() function using the default value of the relevant argument.


library(dslabs)
library (caret)

data(heights)

set.seed(1, sample.kind="Rounding")
test_index<- createDataPartition(heights$height, times = 1, p= 0.5, list=FALSE)
train_set <- heights [-test_index,]
test_set <- heights [test_index,]

kseq<- seq(1, 101,3)


f1_scores <-sapply(kseq, function (i){
  fit <- knn3(train_set$sex ~., data=train_set, k=i)
  y_hat <- predict(fit, test_set, type = "class")
  f_meas <-F_meas(data = y_hat, reference = factor(test_set$sex))})

#What is the max value of F_1?
max(f1_scores)

#At what value of k does the max occur?
plot(kseq, f1_scores)
kseq[which.max(f1_scores)]

#Next we will use the same gene expression example used in the Comprehension Check:
#Distance exercises. You can load it like this:
library(dslabs)
library(caret)
library(tidyverse)
data("tissue_gene_expression")

#First, set the seed to 1 and split the data into training and test sets with p = 0.5.
#Then, report the accuracy you obtain from predicting tissue type using KNN with k = seq(1, 11, 2)
#using sapply() or map_df(). Note: use the createDataPartition() function outside of sapply() or map_df().

#yo lo logré así. hacerlo en función siempre me daba un bug.
library(dslabs)
library(caret)
library(tidyverse)
data("tissue_gene_expression")


tissue_gene_expression<- data.frame(tissue_gene_expression)

set.seed(1, sample.kind = "Rounding")

testindex_gene <- createDataPartition(tissue_gene_expression$y, times=1, p=0.5, list=FALSE)

train_set <- tissue_gene_expression [-testindex_gene,]

test_set <- tissue_gene_expression [testindex_gene,]

sub_train<- subset(train_set, select= -y)
sub_test<- subset(test_set, select= -y)

y <- train_set$y

x<- sub_train

ks <- seq(1,11,2)

fit_p <- knn3(x,y, k= 11)
yhat_p <- predict(fit_p, sub_test, type="class")

confusionMatrix(data=yhat_p, reference= test_set$y)$overall["Accuracy"]
---------------------------
#el codigo "correcto" era este  
  
  set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
test_index <- createDataPartition(y, list = FALSE)
sapply(seq(1, 11, 2), function(k){
  fit <- knn3(x[-test_index,], y[-test_index], k = k)
  y_hat <- predict(fit, newdata = data.frame(x=x[test_index,]),
                   type = "class")
  mean(y_hat == y[test_index])
})



