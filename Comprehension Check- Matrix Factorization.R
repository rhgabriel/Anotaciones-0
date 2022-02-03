#In this exercise set, we will be covering a topic useful for understanding matrix
#factorization: the singular value decomposition (SVD). SVD is a mathematical result
#that is widely used in machine learning, both in practice and to understand the mathematical
#properties of some algorithms. This is a rather advanced topic and to complete this exercise
#set you will have to be familiar with linear algebra concepts such as matrix multiplication,
#orthogonal matrices, and diagonal matrices.

#The SVD tells us that we can decompose an N * p matrix Y with p < N as

#Y=UDV

#With u and v orthogonal of dimensions N * p and p*p respectively and D a p*p
#diagonal matrix with the values of the diagonal decreasing: d1,1>=d2,2>= dp,p

#In this exercise, we will see one of the ways that this decomposition can be useful.
#To do this, we will construct a dataset that represents grade scores for 100 students
#in 24 different subjects. The overall average has been removed so this data represents
#the percentage point each student received above or below the average test score.
#So a 0 represents an average grade (C), a 25 is a high grade (A+), and a -25 represents
#a low grade (F). You can simulate the data like this:

set.seed(1987)
n <- 100
k <- 8
Sigma <- 64  * matrix(c(1, .75, .5, .75, 1, .5, .5, .5, 1), 3, 3) 
m <- MASS::mvrnorm(n, rep(0, 3), Sigma)
m <- m[order(rowMeans(m), decreasing = TRUE),]
y <- m %x% matrix(rep(1, k), nrow = 1) + matrix(rnorm(matrix(n*k*3)), n, k*3)
colnames(y) <- c(paste(rep("Math",k), 1:k, sep="_"),
                 paste(rep("Science",k), 1:k, sep="_"),
                 paste(rep("Arts",k), 1:k, sep="_"))

#Our goal is to describe the student performances as succinctly as possible.
#For example, we want to know if these test results are all just a random independent
#numbers. Are all students just about as good? Does being good in one subject  
#imply you will be good in another? How does the SVD help with all this?
#We will go step by step to show that with just three relatively small pairs of
#vectors we can explain much of the variability in this 100 * 24 dataset.
--------------------------------------------------------------------------------
#Q1
#You can visualize the 24 test scores for the 100 students by plotting an image:
  my_image <- function(x, zlim = range(x), ...){
    colors = rev(RColorBrewer::brewer.pal(9, "RdBu"))
    cols <- 1:ncol(x)
    rows <- 1:nrow(x)
    image(cols, rows, t(x[rev(rows),,drop=FALSE]), xaxt = "n", yaxt = "n",
          xlab="", ylab="",  col = colors, zlim = zlim, ...)
    abline(h=rows + 0.5, v = cols + 0.5)
    axis(side = 1, cols, colnames(x), las = 2)
  }

my_image(y) 

#How would you describe the data based on this figure?
#The students that test well are at the top of the image and there seem to be three groupings by subject.
--------------------------------------------------------------------------------
#Q2
#You can examine the correlation between the test scores directly like this:

my_image(cor(y), zlim = c(-1,1))
range(cor(y))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

#Which of the following best describes what you see?
#There is correlation among all tests, but higher if the tests are in science and
#math and even higher within each subject. 
--------------------------------------------------------------------------------
#Q3
#Remember that orthogonality means that UTU and VTV are equal to the identity matrix.
#This implies that we can also rewrite the decomposition as:
  
#YV=UD or UTY= DVT

#We can think of YV and UTV as two transformations of Y that preserve the total variability 
#of Y since U and V are orthogonal.
  
#Use the function svd() to compute the svd of y. This function will return U, V 
#and the diagonal entries of D.

s <- svd(y)
names(s)

#You can check that the SVD works by typing:

y_svd <- s$u %*% diag(s$d) %*% t(s$v)
max(abs(y - y_svd))

#Compute the sum of squares of the columns of Y and store them in ss_y. Then compute the
#sum of squares of columns of the transformed YV and store them  in ss_yv.
#Confirm that sum(ss_y) is equal to sum(ss_yv).

#What is the value of sum(ss_y) (and also the value of sum(ss_yv))?

ss_y <- apply(y^2, 2, sum)
ss_yv <- apply((y%*%s$v)^2, 2, sum)
sum(ss_y)
sum(ss_yv) 
--------------------------------------------------------------------------------
#Q4

#We see that the total sum of squares is preserved. This is because V is orthogonal.
#Now to start understanding how YV is useful, plot ss_y against the column number
#and then do the same for ss_yv

  #What do you observe?
  ggplot() +
  geom_point(aes(x=1:24, y=ss_y), color="blue") +
  geom_point(aes(x=1:24, y=ss_yv), color="red")
--------------------------------------------------------------------------------
#Q5
#Now notice that we didn't have to compute ss_yv because we already have the answer.
#How? Remember that YV = UD and because U is orthogonal, we know that the sum of
#squares of the columns of U D are the diagonal entries of D squared. Confirm this
#by plotting the square root of ss_yv versus the diagonal entries of D.
  
#Which of these plots is correct?
library(tidyverse)
  data.frame(x = sqrt(ss_yv), y = s$d) %>%
  ggplot(aes(x,y)) +
  geom_point()
--------------------------------------------------------------------------------
#Q6
#So from the above we know that the sum of squares of the columns of Y (the total
#sum of squares) adds up to the sum of s$d^2 and that the transformation YV gives
#us columns with sums of squares equal to s$d^2. Now compute the percent of the
#total variability that is explained by just the first three columns of YV.
    
#What proportion of the total variability is explained by the first three columns of YV?
    
sum(s$d[1:3]^2) / sum(s$d^2)   

#We see that almost 99% of the variability is explained by the first three columns of
#YV=UD. So we get the sense that we should be able to explain much of the variability
#and structure we found while exploring the data with a few columns.  
---------------------------------------------------------------------------------
#Q7
#Before we continue, let's show a useful computational trick to avoid creating 
#the matrix diag(s$d). To motivate this, we note that if we write U out in its columns 
# [U1, U2, Up] then UD is equal to
  
#UD = [U1d1,1, U2d2,2, Up]

#Use the sweep function to compute UD without constructing diag(s$d) or using matrix multiplication.
#Which code is correct?    
  identical(s$u %*% diag(s$d), sweep(s$u, 2, s$d, FUN = "*"))
--------------------------------------------------------------------------------
# Q8.
#We know that U1d1,1, the first column of UD, has the most variability of all the columns of UD.
# Earlier we looked at an image of  using my_image(y), in which we saw that the student to student 
# variability is quite large and that students that are good in one subject tend to be good in all. 
# This implies that the average (across all subjects) for each student should explain a lot of the 
# variability. Compute the average score for each student, plot it against U1d1,1, and 
# describe what you find. What do you observe?

plot (s$u[,1]*s$d[1], rowMeans(y))
         
#There is a linear relationship between the average score for each student and U1d1,1.
--------------------------------------------------------------------------------
#Q9.
#We note that the signs in SVD are arbitrary because: UDV^T = (-U)Dz(-V)^T
# With this in mind we see that the first column of UD is almost identical to the average score for each
# student except for the sign. This implies that multiplying Y by the first column of V must be 
# performing a similar operation to taking the average. Make an image plot of V and describe the first 
# column relative to others and how this relates to taking an average.
# How does the first column relate to the others, and how does this relate to taking an average?

 my_image(s$v)
  dim(s$v) 

#The first column is very close to being a constant, which implies that the first
#column of YV is the sum of the rows of Y multiplied by some constant, and is thus
#proportional to an average. 
  
