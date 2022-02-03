library(dslabs)
library(tidyverse)
data("tissue_gene_expression")
dim(tissue_gene_expression$x)

#Q1
#We want to explore the tissue_gene_expression predictors by plotting them.
#We want to get an idea of which observations are close to each other, but, as
#you can see from the dimensions, the predictors are 500-dimensional, making
#plotting difficult. Plot the first two principal components with color
#representing tissue type.
#Which tissue is in a cluster by itself?

pc <- prcomp(tissue_gene_expression$x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()
--------------------------------------------------------------------------------
#Q2
#The predictors for each observation are measured using the same device and
#experimental procedure. This introduces biases that can affect all the predictors
#from one observation. For each observation, compute the average across all predictors,
#and then plot this against the first PC with color representing tissue. Report the correlation.
#What is the correlation?

avgs <- rowMeans(tissue_gene_expression$x)
data.frame(pc_1 = pc$x[,1], avg = avgs, 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(avgs, pc_1, color = tissue)) +
  geom_point()
cor(avgs, pc$x[,1])
--------------------------------------------------------------------------------
#Q3
#We see an association with the first PC and the observation averages.
#Redo the PCA but only after removing the center. Part of the code is provided for you.
  
x <- with(tissue_gene_expression, sweep(x, 1, rowMeans(x)))
pc <- prcomp(x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()
--------------------------------------------------------------------------------
#Q4
#For the first 10 PCs, make a boxplot showing the values for each tissue.
#For the 7th PC, which two tissues have the greatest median difference?
  for(i in 1:10){
    boxplot(pc$x[,i] ~ tissue_gene_expression$y, main = paste("PC", i))
  }
--------------------------------------------------------------------------------
#Q5
#Plot the percent variance explained by PC number. Hint: use the summary function.
#How many PCs are required to reach a cumulative percent variance explained greater than 50%?
  
  imp_df <- data.frame(summary(pc)$imp)
imp_df <- imp_df[2,] %>% 
  gather(key = pc, value = imp)
imp_df <- imp_df %>% 
  mutate(pc_index = as.integer(str_remove(imp_df$pc, "PC")))
imp_df$pc <- factor(imp_df$pc, 
                    levels = imp_df$pc[order(imp_df$pc_index)])
imp_df <- imp_df %>% mutate(cum_sum = cumsum(imp))

# Then he used ggplot(), with the PCs on the x and the cumulative sum on the y.

imp_df %>% filter(pc_index < 20) %>% arrange(pc_index, cum_sum) %>%
  ggplot(aes(x = pc, y = cum_sum, fill=pc)) +
  geom_col() + scale_y_continuous(breaks = seq(0,1,0.1)) + theme_grey()


plot(summary(pc)$importance[3,])