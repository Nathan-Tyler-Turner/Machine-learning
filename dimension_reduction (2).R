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
# PCA in detail: using covariance
# Two variables, simulated data
# ================================================ 

rm(list=ls())
graphics.off()


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
     xlab="x", ylab="y",
     xlim=c(-1,1)) # plot raw data

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

re$sdev[1]^2 / sum((re$sdev)^2)




# 6. loadings
plot(re$loadings[1,1], re$loadings[1,2], 
     asp=1,
     xlim=c(min(re$loadings[,1]), max(re$loadings[,1])), 
     ylim=c(min(re$loadings[,2]), max(re$loadings[,2])), 
     pch=16, cex=1, col="blue",
     xlab="PC1", ylab="PC2",
     main="Loadings plot") # loadings plot
points(re$loadings[2,1], re$loadings[2,2], 
       pch=16, cex=1, col="red")
text(x=re$loadings[,1], y=re$loadings[,2],
     labels=c("1", "2"),
     pos=3, offset=0.3)

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







# 9. PCA and linear regression
re_lm_raw <- lm(s[,2] ~ s[,1])
re_lm_raw$coefficients

points(s[,1], re_lm_raw$fitted.values,
       pch=16, cex=1, col="green")




re_lm_afterPCA <- lm(s_reduced[,2] ~ s_reduced[,1])
re_lm_afterPCA$coefficients













# ================================================
# PCA in detail: using correlation
# Two variables, simulated data
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
# PCA in detail: using correlation
# Many variables, real data
# ================================================ 

setwd("/Users/Tyler/Documents")


# 1. Import data
in_file_name <- "SCLC_study_output.csv"

# 
# dataIn <- read.csv(file=in_file_name,
#                    header=T,
#                    sep=",")

dataIn <- read.csv(in_file_name)

# rows are peaks, columns are values associated with each sample
all_col_names <- colnames(dataIn)

# remove dots in column names with a white space
all_col_names <- gsub(pattern="\\.", replacement="_", x=all_col_names, perl=T)
colnames(dataIn) <- all_col_names


variable_names <- dataIn[, 1]



# extract the peak area columns
tf <- grepl(pattern="NSCLC_", x=all_col_names) & grepl(pattern="area", x=all_col_names)
II <- which(tf==T)

tf <- grepl(pattern="^SCLC_", x=all_col_names) & grepl(pattern="area", x=all_col_names)
JJ <- which(tf==T)



# get the peak area data
data <- cbind(dataIn[, II], dataIn[, JJ])
sample_names <- colnames(data)



crop_name <- function(s) {
  ind <- regexpr(pattern="_POS", text=s)
  return(substr(x=s, start=1, stop=ind-1))
}

sample_names_cropped <- sapply(sample_names, crop_name)
colnames(data) <- sample_names_cropped





# 2. Filter variables
tf <- grepl(pattern="row_number_of_detected_peaks", x=all_col_names)
II <- which(tf==T)


JJ <- which(dataIn[, II] >= 36) # select variables that are detected in all of the samples
data_for_analysis <- data[JJ, ]
data_for_analysis <- as.data.frame(t(data_for_analysis))
colnames(data_for_analysis) <- variable_names[JJ]



out_file_name <- "SCLC_study_output_filtered.csv"
write.csv(file=out_file_name, x=data_for_analysis)







# 3. PCA using princomp
rm(list=ls())



in_file_name = "SCLC_study_output_filtered.csv"
data_in <- read.csv(file=in_file_name, 
                    header=T,
                    colClasses=c("character", rep("numeric", 19)),
                    check.names = F,
                    row.names=1)


data_for_analysis <- data_in

re <- princomp(data_for_analysis, cor=F)

# scree plot
plot(re$sdev^2,
     pch=16, cex=1, col="blue",
     main="Scree plot",
     xlab="PCA index", ylab="eigenvalues")

variance_all <- re$sdev^2
sum(variance_all[1:2]) / sum(variance_all)


# scores plot
plot(re$scores[,1], re$scores[,2],
     pch=16, cex=1, col="blue",
     xlab="PC1", ylab="PC2",
     main="PCA scores plot")

points(re$scores[21:40,1], re$scores[21:40,2],
     pch=16, cex=1, col="red")

legend_text <- c("NSCLC", "SCLC")
legend("topright", legend=legend_text,
       col=c("blue", "red"),
       pch=16, cex=1)


# loadings plot
plot(re$loadings[, 1], re$loadings[, 2],
     asp=1,
     pch=16, cex=1, col="blue",
     xlab="PC1", ylab="PC2",
     main="Loadings plot")


text(x=re$loadings[, 1], y=re$loadings[, 2],
     labels=colnames(data_for_analysis),
     pos=3,
     offset=0.3)
# The loadings plot shows that variables 1381 and 1076 contribute the most to PC1




# variance of all variables
variance_all_variables <- diag(cov(data_for_analysis))
plot(variance_all_variables, 
     pch=16, cex=1, col="blue",
     xlab="variable index", ylab="variance")

text(x=1:length(variance_all_variables), y=variance_all_variables,
     labels=colnames(data_for_analysis),
     pos=3,
     offset=0.3)






# 4. PCA using prcomp
rm(list=ls())



in_file_name = "SCLC_study_output_filtered.csv"
data_in <- read.csv(file=in_file_name, 
                    header=T,
                    colClasses=c("character", rep("numeric", 19)),
                    check.names = F,
                    row.names=1)


data_for_analysis <- data_in

re <- prcomp(x=data_for_analysis, 
             scale=F)


# scree plot
plot(re$sdev^2,
     pch=16, cex=1, col="blue",
     main="Scree plot",
     xlab="PCA index", ylab="eigenvalues")

variance_all <- re$sdev^2
sum(variance_all[1:2]) / sum(variance_all)


# scores plot
plot(re$x[,1], re$x[,2],
     pch=16, cex=1, col="blue",
     xlab="PC1", ylab="PC2",
     main="PCA scores plot")

points(re$x[21:40,1], re$x[21:40,2],
       pch=16, cex=1, col="red")

legend_text <- c("NSCLC", "SCLC")
legend("topright", legend=legend_text,
       col=c("blue", "red"),
       pch=16, cex=1)


# loadings plot
plot(re$rotation[, 1], re$rotation[, 2],
     asp=1,
     pch=16, cex=1, col="blue",
     xlab="PC1", ylab="PC2",
     main="Loadings plot")


text(x=re$rotation[, 1], y=re$rotation[, 2],
     labels=colnames(data_for_analysis),
     pos=3,
     offset=0.3)


