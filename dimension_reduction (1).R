# dimension_reduction.R


# Author: Xiuxia Du And Tyler Turner






rm(list=ls())
graphics.off()
 





install.packages("ggplot2")
library(ggplot2)
















# ===============================================================================
# A first glimpse at PCA
# ===============================================================================

# 1. generate the raw data

N <- 1000
x <- runif(n=N)

k <- 5 # 0.05
y <- k*x + rnorm(n=N, mean=0, sd=0.3)
# consider the data consisting of N samples and each sample is characterized by two variables





# 2. visualize the raw data
plot(x, y, 
     asp=1, 
     pch=16, cex=1,
     col="blue",
     main="raw data and PC axis", 
     xlab="x", ylab="y") # plot raw data






# 3. do PCA
s <- cbind(x, y)
re <- princomp(s, cor=F)







# 4. PCA looks at the original data in a different coordinate system
# the PC1-PC2 coordinate system
points(c(0, re$loadings[1,1]), c(0, re$loadings[2,1]), 
       type="l", col="green", lwd=4) # add PC1 axis 
points(c(0, re$loadings[1,2]), c(0, re$loadings[2,2]), 
       type="l", col="green", lwd=4) # add PC2 axis






# 5. percentage of variance preserved after discarding PC2
re$sdev[1]^2 / sum(re$sdev^2)







# 6. PCA on completely random data
y <- runif(n=N)


plot(x, y, 
     asp=1, 
     pch=16, cex=1,
     col="blue",
     main="raw data and PC axis", 
     xlab="x", ylab="y") # plot raw data


s <- cbind(x, y)
re <- princomp(s, cor=F)


points(c(0, re$loadings[1,1]), c(0, re$loadings[2,1]), 
       type="l", col="green", lwd=4) # add PC1 axis 
points(c(0, re$loadings[1,2]), c(0, re$loadings[2,2]), 
       type="l", col="green", lwd=4) # add PC2 axis



re$sdev[1]^2 / sum(re$sdev^2)










# ================================================
# PCA in detail: PCA using covariance
# ================================================ 

# 1. generate the raw data

N <- 1000
x <- runif(n=N)

k <- 5 # 0.05
y <- k*x + rnorm(n=N, mean=0, sd=0.3)








# 2. PCA by R
s <- cbind(x, y)
re <- princomp(s, cor=F)
str(re)




# 3. visualize the raw data and the principal components

# raw data in the original coordinate systems
plot(x, y, asp=1, 
     pch=16, cex=1, col="blue",
     main="raw data and PC axis", 
     xlab="x", ylab="y") # plot raw data

# the PC1-PC2 coordinate system
points(c(0, re$loadings[1,1]), c(0, re$loadings[2,1]), 
       type="l", col="green", lwd=4) # add PC1 axis 
points(c(0, re$loadings[1,2]), c(0, re$loadings[2,2]), 
       type="l", col="green", lwd=4) # add PC2 axis

# The PC1-PC2 axis form a new orthogonal coordinate system, a rotated version of the original coordinate system
sum(re$loadings[,1] * re$loadings[,2]) # show PC1 is orthogonal to PC2





# 4. scores
plot(re$scores[,1], re$scores[,2], asp=1, 
     pch=16, cex=1, col="blue",
     main="PCA scores", 
     xlab="PC1", ylab="PC2") # scores plot

# scores are linear combinations of the original data, weights are the loadings
s_mean_centered <- s - matrix(rep(colMeans(s), times=nrow(s)), nrow=nrow(s), byrow=T)
my_scores <- s_mean_centered %*% re$loadings


# compare my_scores with re$scores to understand how scores are calculated by PCA
my_scores[1:5,]
re$scores[1:5,]
identical(round(my_scores, digits=2), round(re$scores, digits=2))


# scores are uncorrelated
cor(x,y) # correlation between the original x and y
cor(re$scores[,1], re$scores[,2]) # correlation between PC1 and PC2





# 5. variance
# total variance in the data remain the same before and after PCA
total_variance_in_original_data <- var(x) + var(y)
total_variance_in_scores <- var(re$scores[,1]) + var(re$scores[,2])


# total variance is equal to the sum of the eigenvalues
sum(re$sdev^2)


# scree plot -- variance vs PC number
plot(re$sdev^2, 
     pch=16, cex=1, col="blue",
     main="scree plot", 
     xlab="PC index", ylab="eigenvalues")
# variances along PCs are the same as the corresponding eigenvalues





# 6. loadings
plot(re$loadings[1,1], re$loadings[1,2], 
     xlim=c(min(re$loadings[,1]), max(re$loadings[,1])), 
     ylim=c(min(re$loadings[,2]), max(re$loadings[,2])), pch=16, cex=1) # loadings plot
points(re$loadings[2,1], re$loadings[2,2], 
       pch=16, cex=1, col="red")

re$loadings


# the loadings matrix is an orthogonal matrix because its inverse equals to its transpose
all(t(round(re$loadings, digits=2)) == round(solve(re$loadings), digits=2))





# 7. how to reconstruct x from scores and loadings
s_reconstructed <- re$scores %*% t(re$loadings) # back to the original coordinate system

colMeans(s_reconstructed) # still mean centered

s_reconstructed <- s_reconstructed + matrix(rep(colMeans(s), times=nrow(s)), nrow=nrow(s), byrow=T)
# shift back to the original mean

plot(s[,1], s[,2], 
     pch=16, cex=1, col="blue")
points(s_reconstructed[,1], s_reconstructed[,2], 
       pch=16, cex=1, col="red")





# 8. dimension reduction
p_reduced <- re$loadings[,1]
s_reduced <- re$scores[,1] %*% t(p_reduced) +  matrix(rep(colMeans(s), times=nrow(s)), nrow=nrow(s), byrow=T)
points(s_reduced[,1], s_reduced[,2], 
       pch=16, cex=1, col="blue")








# ================================================
# example 1: PCA using correlation
# ================================================ 

rm(list=ls())
graphics.off()





# 1. generate the raw data
N <- 1000
x <- runif(n=N) 

k <- 5
y <- k*x + rnorm(n=N, mean=0, sd=0.3)



s <- cbind(x, y)





# 2. PCA by R
re <- princomp(s, cor=T)





# 3. visualize the raw data and the principal components
plot(x, y, asp=1, 
     pch=16, cex=1, col="blue",
     main="raw data and PC axis", 
     xlab="x", ylab="y") # plot raw data


# the PC1-PC2 coordinate system
points(c(0, re$loadings[1,1]), c(0, re$loadings[2,1]), 
       type="l", col="green", lwd=4) # add PC1 axis 
points(c(0, re$loadings[1,2]), c(0, re$loadings[2,2]), 
       type="l", col="green", lwd=4) # add PC2 axis

# The PC1-PC2 axis form a new orthogonal coordinate system, a rotated version of the original x-y coordinate system
sum(re$loadings[,1] * re$loadings[,2]) # show PC1 is orthogonal to PC2


# why do the PC1-PC2 axis are not aligned with the data?

# compute the z-score of x and y
x_zscore <- (x - mean(x))/sd(x)
y_zscore <- (y - mean(y))/sd(y)

plot(x_zscore, y_zscore, asp=1, 
     pch=16, cex=1, col="blue",
     main="z_score transformed data and PC axis", 
     xlab="x", ylab="y") # plot raw data

# the PC1-PC2 coordinate system
points(c(0, re$loadings[1,1]), c(0, re$loadings[2,1]), 
       type="l", col="green", lwd=4) # add PC1 axis 
points(c(0, re$loadings[1,2]), c(0, re$loadings[2,2]), 
       type="l", col="green", lwd=4) # add PC2 axis





# 4. scores
plot(re$scores[,1], re$scores[,2], asp=1, 
     pch=16, cex=1, col="blue",
     main="scores", xlab="PC1", ylab="PC2") # scores plot

# scores are linear combinations of the z_score transformed data, weights are the loadings
my_scores <- cbind(x_zscore, y_zscore) %*% re$loadings

# scores are uncorrelated
cor(x_zscore, y_zscore) # correlation between the original x and y
cor(re$scores[,1], re$scores[,2]) # correlation between PC1 and PC2





# 5. variance
# total variance in the data remain the same before and after PCA
total_variance_in_zscore_transformed_data <- var(x_zscore) + var(y_zscore)
total_variance_in_scores <- var(re$scores[,1]) + var(re$scores[,2])

# scree plot -- variance vs PC number
plot(re$sdev^2, 
     pch=16, cex=1, col="blue",
     main="scree plot", 
     xlab="PC number", ylab="eigenvalues")





# 6. loadings
plot(re$loadings[1,1], re$loadings[1,2], 
     pch=16, cex=1, col="blue",
     xlim=c(min(re$loadings[,1]), max(re$loadings[,1])), 
     ylim=c(min(re$loadings[,2]), max(re$loadings[,2]))) # loadings plot
points(re$loadings[2,1], re$loadings[2,2], 
       pch=16, cex=1, col="red")


# the loadings matrix is an orthogonal matrix
all(round(t(re$loadings), digits=2) == round(solve(re$loadings), digits=2))






# 7. how to reconstruct original data
temp <- re$scores %*% t(re$loadings)
x_reconstructed <- temp[,1] * sd(x) + mean(x)
y_reconstructed <- temp[,2] * sd(y) + mean(y)
plot(x, y,
     pch=16, cex=1)

points(x_reconstructed, y_reconstructed, 
     pch=16, cex=1,
     col="red")





# 8. dimension reduction
p_reduced <- re$loadings[,1]
s_reduced <- re$scores[,1] %*% t(p_reduced)

x_reconstructed_reduced <- s_reduced[,1] * sd(x) + mean(x)
y_reconstructed_reduced <- s_reduced[,2] * sd(y) + mean(y)
points(x_reconstructed_reduced, y_reconstructed_reduced,
       pch=16, cex=1,
       col="blue")

















# ================================================
# example 2: faahKO data
# ================================================

path_to_R_libs <- "/Users/xdu4/Documents/Duxiuxia/R_libs"

packageName <- "faahKO"
if (!is.element(el=packageName, set=installed.packages(lib.loc=path_to_R_libs)[,1])) {
    install.packages(pkgs=packageName,
                     lib=path_to_R_libs,
                     repos="http://cran.us.r-project.org",
                     dependencies=T)
}
library(package=packageName,
        lib.loc=path_to_R_libs,
        character.only = T)


source("http://bioconductor.org/biocLite.R")
biocLite("faahKO")
library(faahKO)

cdfpath <- file.path(find.package(package="faahKO"), "cdf")
list.files(cdfpath, recursive = TRUE)

show(faahko)







