library(dslabs)
library(caret)
library(tidyverse)
data("tissue_gene_expression")

set.seed(1993, sample.kind="Rounding") # if using R 3.6 or later
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

#Use the train() function to estimate the accuracy of LDA.
#For this question, use the version of x and y created with the code above:
#do not split them or tissue_gene_expression into training and test sets (understand this can lead to overfitting).
#Report the accuracy from the train() results (do not make predictions).

fit_lda <- train(x, y, method = "lda")
fit_lda$results["Accuracy"]

#In this case, LDA fits two 10-dimensional normal distributions.
#Look at the fitted model by looking at the finalModel component of the result of train()
#Notice there is a component called means that includes the estimated means of both distributions.
#Plot the mean vectors against each other and determine which predictors (genes) appear to be driving the algorithm. 

t(fit_lda$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label = predictor_name)) +
  geom_point() +
  geom_text() +
  geom_abline()

#Repeat the exercise in Q1 with QDA.

#Create a dataset of samples from just cerebellum and hippocampus,
#two parts of the brain, and a predictor matrix with 10 randomly selected columns using the following code:

set.seed(1993, sample.kind="Rounding")
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

#Use the train() function to estimate the accuracy of QDA.
#For this question, use the version of x and y created above 
#instead of the default from tissue_gene_expression.
#Do not split them into training and test sets (understand this can lead to overfitting).
#What is the accuracy?

fit_qda <- train(x, y, method = "qda")
fit_qda$results["Accuracy"]

#Which TWO genes drive the algorithm when using QDA instead of LDA (i.e. the two genes with the highest means)?

t(fit_qda$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label = predictor_name)) +
  geom_point() +
  geom_text() +
  geom_abline()

#One thing we saw in the previous plots is that the values of the predictors
#correlate in both groups: some predictors are low in both groups and others
#high in both groups. The mean value of each predictor found in colMeans(x) 
#is not informative or useful for prediction and often for purposes of interpretation,
#it is useful to center or scale each column. 
#This can be achieved with the preProcess argument in train().
#Re-run LDA with preProcess = "center". Note that accuracy does not change,
#but it is now easier to identify the predictors that differ more between groups than based
#on the plot made in Q2.

fit_lda <- train(x, y, method = "lda", preProcess = "center")
fit_lda$results["Accuracy"]

t(fit_lda$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label = predictor_name)) +
  geom_point() +
  geom_text() +
  geom_abline()

#Now we are going to increase the complexity of the challenge slightly.
#Repeat the LDA analysis from Q5 but using all tissue types.
#Use the following code to create your dataset:

set.seed(1993, sample.kind="Rounding") 
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
x <- x[, sample(ncol(x), 10)]

fit_lda <- train(x, y, method = "lda", preProcess = c("center"))
fit_lda$results["Accuracy"]
