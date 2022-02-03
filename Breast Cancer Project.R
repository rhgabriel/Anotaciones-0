#Breast cancer project part 1

#The brca dataset from the dslabs package contains information about breast cancer
#diagnosis biopsy samples for tumors that were determined to be either benign
#(not cancer) and malignant (cancer). The brca object is a list consisting of

#brca$y: a vector of sample classifications ("B" = benign or "M" = malignant)
#brca$x: a matrix of numeric features describing properties of the shape and size
#of cell nuclei extracted from biopsy microscope images

#For these exercises, load the data by setting your options and loading the 
#libraries and data as shown in the code here:

options(digits = 3)
library(matrixStats)
library(tidyverse)
library(caret)
library(dslabs)
data(brca)

--------------------------------------------------------------------------------
#Question 1: Dimensions and properties

#How many samples are in the dataset?
dim(brca$x)
#How many predictors are in the matrix?
 dim(brca$x)  
#What proportion of the samples are malignant?
  mean(brca$y =="M")
#Which column number has the highest mean?
 which.max(colMeans(brca$x))
#Which column number has the lowest standard deviation?
which.min(colSds(brca$x))
--------------------------------------------------------------------------------
#Q2: Scaling the matrix
  
#Use sweep() two times to scale each column: subtract the column means of brca$x,
#then divide by the column standard deviations of brca$x.
brca_x <- as.data.frame(unlist(brca[["x"]]))
scale <- function(x){
  (x-mean(x))/sd(x)
  (x-mean(x))/sd(x)
}
brca_x1 <-brca_x %>% mutate_all(scale)


#or it can be done by sweep function like
x_centered <- sweep(brca$x, 2, colMeans(brca$x))
x_scaled <- sweep(x_centered, 2, colSds(brca$x), FUN = "/")
#x_scaled$y <- as.vector(unlist(brca[["y"]]))
  
#After scaling, what is the standard deviation of the first column?
sd(brca_x1[,1])
#After scaling, what is the median value of the first column?
median(brca_x1[,1])
--------------------------------------------------------------------------------
#Q3: Distance
  
#Calculate the distance between all samples using the scaled matrix.
#What is the average distance between the first sample, which is benign, and other
#benign samples?
  d_samples <- dist(x_scaled)
dist_BtoB <- as.matrix(d_samples)[1, brca$y == "B"]
mean(dist_BtoB[2:length(dist_BtoB)])
  
#What is the average distance between the first sample and malignant samples?

dist_BtoM <- as.matrix(d_samples)[1, brca$y == "M"]
mean(dist_BtoM)
--------------------------------------------------------------------------------
#Q4: Heatmap of features
  
#Make a heatmap of the relationship between features using the scaled matrix.
#Which of these heatmaps is correct?
#To remove column and row labels like the images below, use labRow = NA and labCol = NA
  d_features <- dist(t(x_scaled))
heatmap(as.matrix(d_features), labRow = NA, labCol = NA)
--------------------------------------------------------------------------------
#Q5: Hierarchical clustering
  
#Perform hierarchical clustering on the 30 features. Cut the tree into 5 groups.
#All but one of the answer options are in the same group.
#Which is in a different group?
h<-hclust(d_features)
groups <-cutree(h, k= 5)
split(names(groups), groups)
--------------------------------------------------------------------------------
#Part 2
#Q6: PCA proportion of variance 

#Perform a principal component analysis of the scaled matrix.
#What proportion of variance is explained by the first principal component?
#How many principal components are required to explain at least 90% of the variance?
pca <- prcomp(x_scaled)
summary(pca)
--------------------------------------------------------------------------------
#Q7: PCA plotting PCs 
  
#Plot the first two principal components with color representing tumor type (benign/malignant).
#Which of the following is true?

  data.frame(pca$x[,1:2], type = brca$y) %>%
  ggplot(aes(PC1, PC2, color = type)) +
  geom_point()
--------------------------------------------------------------------------------
#Q8: PC boxplot
#Make a boxplot of the first 10 PCs grouped by tumor type.
#Which PCs are significantly different enough by tumor type that there is no
#overlap in the interquartile ranges (IQRs) for benign and malignant samples?
  data.frame(type = brca$y, pca$x[,1:10]) %>%
  gather(key = "PC", value = "value", -type) %>%
  ggplot(aes(PC, value, fill = type)) +
  geom_boxplot()
--------------------------------------------------------------------------------
#Part 3
#Set the seed to 1, then create a data partition splitting brca$y and the scaled
#version of the brca$x matrix into a 20% test set and 80% train using the following code

set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(brca$y, times = 1, p = 0.2, list = FALSE)
test_x <- x_scaled[test_index,]
test_y <- brca$y[test_index]
train_x <- x_scaled[-test_index,]
train_y <- brca$y[-test_index]  
  
#Q9:Training and test sets
#Check that the training and test sets have similar proportions of benign and malignant tumors.
#What proportion of the training set is benign?
summary(train_y)
mean(train_y == "B")
#What proportion of the test set is benign?
summary(test_y)
mean(test_y == "B")
--------------------------------------------------------------------------------
#Q10a: K-means Clustering
#The predict_kmeans() function defined here takes two arguments - a matrix of
#observations x and a k-means object k - and assigns each row of x to a cluster from k.
  
  predict_kmeans <- function(x, k) {
    centers <- k$centers    # extract cluster centers
    # calculate distance to cluster centers
    distances <- sapply(1:nrow(x), function(i){
      apply(centers, 1, function(y) dist(rbind(x[i,], y)))
    })
    max.col(-t(distances))  # select cluster with min distance to center
  }

#Set the seed to 3. Perform k-means clustering on the training set with 2 centers
#and assign the output to k. Then use the predict_kmeans() function to make predictions on the test set.
#What is the overall accuracy?
set.seed(3, sample.kind = "Rounding")
k <- kmeans(train_x, centers = 2)
kmeans_preds <- ifelse(predict_kmeans(test_x, k) == 1, "B", "M")
mean(kmeans_preds == test_y)
--------------------------------------------------------------------------------
#Q10b: K-means Clustering
#What proportion of benign tumors are correctly identified?
#What proportion of malignant tumors are correctly identified?
table(test_y,kmeans_preds)
sensitivity(factor(kmeans_preds), test_y, positive = "B")
sensitivity(factor(kmeans_preds), test_y, positive = "M")
--------------------------------------------------------------------------------
#Q11: Logistic regression model
#Set the seed to 1, then fit a logistic regression model on the training set with
#caret::train() using all predictors. Ignore warnings about the algorithm not converging.
#Make predictions on the test set.
#What is the accuracy of the logistic regression model on the test set?
set.seed(1, sample.kind="Rounding")
train_glm <- train(train_x, train_y,
                   method = "glm")
glm_preds <- predict(train_glm, test_x)
mean(glm_preds == test_y)
--------------------------------------------------------------------------------
#Q12: LDA and QDA models
#Train an LDA model and a QDA model on the training set, setting the seed to 1
#before each model. Make predictions on the test set using each model.
#What is the accuracy of the LDA model on the test set?
#What is the accuracy of the QDA model on the test set?
  set.seed(1, sample.kind="Rounding")
train_LDA <- train(train_x, train_y,
                   method = "lda")
LDA_preds <- predict(train_LDA, test_x)
mean(LDA_preds == test_y)
-----
set.seed(1, sample.kind="Rounding")
train_QDA <- train(train_x, train_y,
                   method = "qda")
QDA_preds <- predict(train_QDA, test_x)
mean(QDA_preds == test_y)
--------------------------------------------------------------------------------
#Q13: Loess model
#Set the seed to 5, then fit a loess model on the training set with the caret package.
#You will need to install the gam package if you have not yet done so. Use the default
#tuning grid. This may take several minutes; ignore warnings. Generate predictions on the test set.
#What is the accuracy of the loess model on the test set?
set.seed(3, sample.kind="Rounding")
train_loess <- train(train_x, train_y,
                   method = "gamLoess")
loess_preds <- predict(train_loess, test_x)
mean(loess_preds == test_y)  
--------------------------------------------------------------------------------
#Part 4
#Q14:K-nearest neighbors model
#Set the seed to 7, then train a k-nearest neighbors model on the training set
#using the caret package. Try odd values of from 3 to 21. Use the final model to
#generate predictions on the test set.
#What is the final value of K used in the model?
#What is the accuracy of the kNN model on the test set? 
set.seed(7, sample.kind="Rounding")
train_knn <- train(train_x, train_y,
                     method = "knn",tuneGrid = data.frame(k = seq(3, 21, 2)))
knn_preds <- predict(train_knn, test_x)
mean(knn_preds == test_y)  
train_knn$bestTune
--------------------------------------------------------------------------------
#Q15a: Random forest model
#Set the seed to 9, then train a random forest model on the training set using 
#the caret package. Test mtry values of c(3, 5, 7, 9). Use the argument
#importance = TRUE so that feature importance can be extracted. Generate predictions on the test set.
#Note: please use c(3, 5, 7, 9) instead of seq(3, 9, 2) in tuneGrid.
#What value of mtry gives the highest accuracy?
#What is the accuracy of the random forest model on the test set?
#What is the most important variable in the random forest model?

set.seed(9, sample.kind="Rounding")
train_rf <- train(train_x, train_y,
                   method = "rf",
                  tuneGrid = data.frame(mtry = c(3,5,7,9)))
rf_preds <- predict(train_rf, test_x)
mean(rf_preds == test_y)  
train_rf$bestTune  
varImp(train_rf)
--------------------------------------------------------------------------------
#Q15b: Random forest model
#Consider the top 10 most important variables in the random forest model.
#Which set of features is most important for determining tumor type?
varImp(train_rf)
--------------------------------------------------------------------------------
#Q16a: Creating an ensemble
#Create an ensemble using the predictions from the 7 models created in the previous
#exercises: k-means, logistic regression, LDA, QDA, loess, k-nearest neighbors,
#and random forest. Use the ensemble to generate a majority prediction of the tumor
#type (if most models suggest the tumor is malignant, predict malignant).
#What is the accuracy of the ensemble prediction?
  model <- c("kmeans_preds","glm_preds","LDA_preds","QDA_preds","rf_preds","knn_preds","loess_preds")
pred <- sapply(1:7, function(x){
  as.factor(get(model[x]))})
dim(pred)
pred <- as.data.frame(pred)
names(pred) <-c("kmeans_preds","glm_preds","lda_preds","qda_preds","rf_preds","knn_preds","loess_preds")
acc <- colMeans(as.matrix(pred)==test_y)
acc
mean(acc)

ensemble <- cbind(glm = glm_preds == "B", lda = LDA_preds == "B", qda = QDA_preds == "B",
                  loess = loess_preds == "B", rf = rf_preds == "B", knn = knn_preds == "B",
                  kmeans = kmeans_preds == "B")

ensemble_preds <- ifelse(rowMeans(ensemble) > 0.5, "B", "M")
mean(ensemble_preds == test_y)
--------------------------------------------------------------------------------
#Q16b: Creating an ensemble
#Make a table of the accuracies of the 7 models and the accuracy of the ensemble model.
#Which of these models has the highest accuracy?
  
  model <- c("kmeans_preds","glm_preds","LDA_preds","QDA_preds","rf_preds","knn_preds","loess_preds")
pred <- sapply(1:7, function(x){
  as.factor(get(model[x]))})
dim(pred)
pred <- as.data.frame(pred)
names(pred) <-c("kmeans_preds","glm_preds","lda_preds","qda_preds","rf_preds","knn_preds","loess_preds")
acc <- colMeans(as.matrix(pred)==test_y)
acc  
