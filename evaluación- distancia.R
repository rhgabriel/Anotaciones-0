library(dslabs)
data(tissue_gene_expression)
dim(tissue_gene_expression$x)
table(tissue_gene_expression$y)

#Which of the following lines of code computes the Euclidean distance between
#each observation and stores it in the object d?

d <- dist(tissue_gene_expression$x)

#Using the dataset from Q1, compare the distances between observations 1 and 2 (both cerebellum),
#observations 39 and 40 (both colon), and observations 73 and 74 (both endometrium).
#Distance-wise, are samples from tissues of the same type closer to each other than tissues of different type?

ind <- c(1, 2, 39, 40, 73, 74)
as.matrix(d)[ind,ind]

#Make a plot of all the distances using the image()
#function to see if the pattern you observed in Q2 is general.
#Which code would correctly make the desired plot?

image(as.matrix(d))