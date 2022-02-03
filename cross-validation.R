#Generate a set of random predictors and outcomes using the following code:

library(tidyverse)
library(caret)

# set.seed(1996) #if you are using R 3.5 or earlier
set.seed(1996, sample.kind="Rounding") #if you are using R 3.6 or later
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,sample(p, 100)]

#Because x and y are completely independent, you should not be able to predict
#y using x with accuracy greater than 0.5.
#Confirm this by running cross-validation using logistic regression to fit the model.
#Because we have so many predictors, we selected a random sample x_subset.
#Use the subset when training the model.
#Which code correctly performs this cross-validation?

fit <- train(x_subset, y, method = "glm")
fit$results

#Now, instead of using a random selection of predictors,
#we are going to search for those that are most predictive of the outcome.
#We can do this by comparing the values for the group to those in the group,
#for each predictor, using a t-test. You can perform this step like this:

install.packages("BiocManager")
BiocManager::install("genefilter")
library(genefilter)
tt <- colttests(x, y)

#Which of the following lines of code correctly creates a vector of the p-values called pvals?

pvals <- tt$p.value

#Create an index ind with the column numbers of the predictors that were "statistically significantly"
#associated with y. Use a p-value cutoff of 0.01 to define "statistically significantly."
#How many predictors survive this cutoff?

ind <- which(pvals < 0.01)
length(ind)

#Now re-run the cross-validation after redefinining x_subset to be the subset of x
#defined by the columns showing "statistically significant" association with y.
#What is the accuracy now?

x_subset <- x[ ,sample(ind, 100)]

fit <- train(x_subset, y, method = "glm")
fit$results

#Re-run the cross-validation again, but this time using kNN.
#Try out the following grid k = seq(101, 301, 25) of tuning parameters.
#Make a plot of the resulting accuracies.
#Which code is correct?

fit <- train(x_subset, y, method = "knn", tuneGrid = data.frame(k = seq(101, 301, 25)))
ggplot(fit)

#Use the train() function with kNN to select the best k for predicting tissue from gene
#expression on the tissue_gene_expression dataset from dslabs. Try k = seq(1,7,2) for tuning parameters.
#For this question, do not split the data into test and train sets
#(understand this can lead to overfitting, but ignore this for now).
#What value of k results in the highest accuracy?

library(dslabs)
data("tissue_gene_expression")

fit <- with(tissue_gene_expression, train(x, y, method = "knn", tuneGrid = data.frame( k = seq(1, 7, 2))))
ggplot(fit)
fit$results
