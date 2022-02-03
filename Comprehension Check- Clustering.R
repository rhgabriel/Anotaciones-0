#These exercises will work with the tissue_gene_expression dataset, which is part of the dslabs package.
library(dslabs)
library(tidyverse)
data(tissue_gene_expression)
--------------------------------------------------------------------------------
#Q1
#Load the tissue_gene_expression dataset. Remove the row means and compute the
#distance between each observation. Store the result in d.
#Which of the following lines of code correctly does this computation?

d <- dist(tissue_gene_expression$x - rowMeans(tissue_gene_expression$x))
--------------------------------------------------------------------------------
#Q2
#Make a hierarchical clustering plot and add the tissue types as labels.
#You will observe multiple branches.
#Which tissue type is in the branch farthest to the left?
  
h <- hclust(d)
plot(h, cex= 0.65, main = "", xlab="")
--------------------------------------------------------------------------------
#Q3
#Select the 50 most variable genes. Make sure the observations show up in the
#columns, that the predictor are centered, and add a color bar to show the
#different tissue types. Hint: use the ColSideColors argument to assign colors.
#Also, use col = RColorBrewer::brewer.pal(11, "RdBu") for a better use of colors.
  
#Part of the code is provided for you here:
#Which line of code should replace BLANK
install.packages("matrixStats")
library(matrixStats)  
library(RColorBrewer)
sds <- matrixStats::colSds(tissue_gene_expression$x)
ind <- order(sds, decreasing = TRUE)[1:50]
colors <- brewer.pal(7, "Dark2")[as.numeric(tissue_gene_expression$y)]
heatmap(t(tissue_gene_expression$x[,ind]), col = brewer.pal(11, "RdBu"), scale = "row", ColSideColors = colors)