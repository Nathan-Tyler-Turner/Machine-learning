# clustering.R


# Author:Xiuxia Du And Tyler Turner








# =======================================
# kmeans
# =======================================
rm(list=ls())
graphics.off()


# 1. kmeans on toy data
data_in <- matrix(c(1, 1, 2, 1, 4, 3, 5, 4), ncol=2, byrow=T)

re <- kmeans(data_in, centers=2, algorithm="Lloyd")




# 2. kmeans on read biological data




path_to_R_libs <- "/Users/xdu4/Documents/Duxiuxia/R_libs"
setwd("/Users/xdu4/Google Drive/Ocean_2/Teaching/ML/2015_Fall/Lectures/R")







par_default <- par()
par(pin=c(3,3))
par(mar=c(1,1,1,1))








# =======================================
# prepare data
# =======================================
# 1. Import data
in_file_name <- "SCLC_study_output.csv"


dataIn <- read.csv(file=in_file_name,
                   header=T,
                   sep=",")



# rows are peaks, columns are values associated with each sample
all_col_names <- colnames(dataIn)

# remove dots in column names with a white space
all_col_names <- gsub(pattern="\\.", replacement=" ", x=all_col_names, perl=T)
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
tf <- grepl(pattern="row number of detected peaks", x=all_col_names)
II <- which(tf==T)


JJ <- which(dataIn[, II] == 40) # select variables that are detected in all of the samples
data_for_analysis <- data[JJ, ]
data_for_analysis <- as.data.frame(t(data_for_analysis))
colnames(data_for_analysis) <- variable_names[JJ]





# =======================================
# kmeans clustering
# =======================================
re.kmeans <- kmeans(x=data_for_analysis, centers=data_for_analysis[1:2,], algorithm="Lloyd")
re.kmeans$size




# PCA
re.PCA <- prcomp(x=data_for_analysis, scale=T)
plot(re.PCA$sdev^2,
     pch=16, cex=1,
     xlab="PC number", ylab="variance",
     main="scree plot after standardization of variables")


# kmeans clustering of PCA scores
re.PCA.kmeans <- kmeans(x=re.PCA$x, centers=2, algorithm="Lloyd")
re.PCA.kmeans$size


# kmeans clustering of certain PCA scores
re.PCA.kmeans <- kmeans(x=re.PCA$x[, 1:5], centers=2, algorithm="Lloyd")
re.PCA.kmeans$size
# Since the argument centers is a number, a random set of (distinct) rows in x is chosen as the initial centers.
# This can cause the clustering results to be different when kmeans() is called multiple times.







# =======================================
# kmedoids clustering
# =======================================

packageName <- "cluster"
if (!is.element(el=packageName, set=installed.packages(lib.loc=path_to_R_libs)[,1])) {
    install.packages(pkgs=packageName, 
                     lib=path_to_R_libs, 
                     repos="http://cran.us.r-project.org",
                     dependencies=T)
}
library(package=packageName,
        lib.loc=path_to_R_libs,
        character.only=T)





# pam clustering of the original data
re.pam <- pam(x=data_for_analysis, k=2, diss=F)
re.pam$clustering[1:20]
re.pam$clustering[21:40]
# NSCLC_H358_2, NSCLC_H522_1, NSCLC_H522_2, NSCLC_PC9_1 are wrongly clustered





# pam clustering of the PCA scores
re.PCA.pam <- pam(x=re.PCA$x, k=2, diss=F)
re.PCA.pam$clustering[1:20]
re.PCA.pam$clustering[21:40]
# multiple files are wrongly clustered





# pam clustering of PC1
re.PCA.pam <- pam(x=re.PCA$x[,1], k=2, diss=F)
re.PCA.pam$clustering[1:20]
re.PCA.pam$clustering[21:40]
# perfect clustering




# observe PC1: grouped
plot(re.PCA$x[, 1], rep(0, times=length(re.PCA$x[, 1])),
     pch=16, cex=1,
     main="PC1 after standardization of variables",
     xlab="score", ylab="")
points(re.PCA$x[21:40, 1], rep(0, times=length(re.PCA$x[21:40, 1])),
       pch=16, cex=1,
       col="red")
legend("topright", legend=c("NSCLC", "SCLC"), pch=c(16, 16), col=c("black", "red"))







# pam clustering of PC1 and PC2
re.PCA.pam <- pam(x=re.PCA$x[,1:2], k=2, diss=F)
re.PCA.pam$clustering[1:20]
re.PCA.pam$clustering[21:40]
plot(re.PCA$x[, 1], re.PCA$x[, 2],  pch=16, cex=1,
     xlab="PC1", ylab="PC2",
     main="PC2 vs. PC1")
points(re.PCA$x[21:40, 1], re.PCA$x[21:40, 2], pch=16, cex=1, col="red")
points(c(-0.4, -0.4), c(-3, 5), type="l", lty=2, lwd=4, col="blue")

# observe PC2: not separated
re.PCA.pam <- pam(x=re.PCA$x[,2], k=2, diss=F)
re.PCA.pam$clustering[1:20]
re.PCA.pam$clustering[21:40]





# pam clustering of PC1, PC2, and PC3
re.PCA.pam <- pam(x=re.PCA$x[,1:3], k=2, diss=F)
re.PCA.pam$clustering[1:20]
re.PCA.pam$clustering[21:40]





# silhoutte
potential_k <- 2:5
avgSilhoutteWidth <- vector(mode="numeric", length=0)
for (i in 1:length(potential_k)) {
    re.pam <- pam(data_for_analysis, k=potential_k[i], diss=F)
    
    avgSilhoutteWidth <- c(avgSilhoutteWidth, re.pam$silinfo$avg.width)
}
plot(avgSilhoutteWidth, pch=16, cex=1)





# =======================================
# hierarchical clustering
# =======================================
# euclidean dissimilarity
distMatrix <- dist(x=data_for_analysis, method="euclidean")


# complete linkage
re.hclust <- hclust(d=distMatrix, method="complete")

# vertical dendrogram
plot(re.hclust, main="dist = euclidean, linkage = complete", sub="",
     xlab="", ylab="dissimilarity", 
     hang=-1)
cutoff <- 2.5e+6
points(c(0, nrow(data_for_analysis)), c(cutoff, cutoff), 
       type="l", lwd=2, lty=2,
       col="red")

# horizontal dendrogram
plot(as.dendrogram(re.hclust), horiz=T, 
     main="dist = euclidean, linkage = complete", 
     xlab="dissimilarity", ylab="")
points(c(cutoff, cutoff), c(0, nrow(data_for_analysis)), 
       type="l", lwd=2, lty=2,
       col="red")



re.hclust$order

# first merge
re.hclust$merge[1,]
re.hclust$labels[c(27,28)]
merge_dist <- re.hclust$height[1]
points(c(merge_dist, merge_dist), c(0, nrow(data_for_analysis)),
       type="l", lwd=2, lty=2,
       col="green")


# second merge
re.hclust$merge[2,]
re.hclust$labels[c(25,26)]
merge_dist <- re.hclust$height[2]
points(c(merge_dist, merge_dist), c(0, nrow(data_for_analysis)),
       type="l", lwd=2, lty=2,
       col="green")

sqrt(sum((data_for_analysis[25,]-data_for_analysis[26,])^2))

# third merge
re.hclust$merge[3,]
re.hclust$labels[c(18,19)]
merge_dist <- re.hclust$height[3]
points(c(merge_dist, merge_dist), c(0, nrow(data_for_analysis)),
       type="l", lwd=2, lty=2,
       col="green")


# fourth merge
re.hclust$merge[4,]
re.hclust$labels[c(5,8)]
merge_dist <- re.hclust$height[4]
points(c(merge_dist, merge_dist), c(0, nrow(data_for_analysis)),
       type="l", lwd=2, lty=2,
       col="green")


# fifth merge
re.hclust$merge[5,]
re.hclust$labels[c(23,24)]
merge_dist <- re.hclust$height[5]
points(c(merge_dist, merge_dist), c(0, nrow(data_for_analysis)),
       type="l", lwd=2, lty=2,
       col="green")


# sixth merge
re.hclust$merge[6,]
re.hclust$labels[c(6, 7)]
merge_dist <- re.hclust$height[6]
points(c(merge_dist, merge_dist), c(0, nrow(data_for_analysis)),
       type="l", lwd=2, lty=2,
       col="green")


# seventh merge
re.hclust$merge[7,]
re.hclust$labels[c(29, 31)]
merge_dist <- re.hclust$height[7]
points(c(merge_dist, merge_dist), c(0, nrow(data_for_analysis)),
       type="l", lwd=2, lty=2,
       col="green")


# eighth merge
re.hclust$merge[8,]
re.hclust$labels[c(21, 22)]
merge_dist <- re.hclust$height[8]
points(c(merge_dist, merge_dist), c(0, nrow(data_for_analysis)),
       type="l", lwd=2, lty=2,
       col="green")


# ninth merge
re.hclust$merge[9,]
re.hclust$labels[c(2, 3)]
merge_dist <- re.hclust$height[9]
points(c(merge_dist, merge_dist), c(0, nrow(data_for_analysis)),
       type="l", lwd=2, lty=2,
       col="green")


# tenth merge
re.hclust$merge[10,]
re.hclust$labels[35]
re.hclust$merge[1,]
re.hclust$labels[c(27, 28)]
merge_dist <- re.hclust$height[10]
points(c(merge_dist, merge_dist), c(0, nrow(data_for_analysis)),
       type="l", lwd=2, lty=2,
       col="magenta")


# eleventh merge
re.hclust$merge[11,]
re.hclust$labels[c(1, 20)]
merge_dist <- re.hclust$height[11]
points(c(merge_dist, merge_dist), c(0, nrow(data_for_analysis)),
       type="l", lwd=2, lty=2,
       col="magenta")


# twelfth merge
re.hclust$merge[12,]
re.hclust$labels[c(10, 16)]
merge_dist <- re.hclust$height[12]
points(c(merge_dist, merge_dist), c(0, nrow(data_for_analysis)),
       type="l", lwd=2, lty=2,
       col="magenta")








# single linkage
re.hclust <- hclust(d=distMatrix, method="single")

# vertical dendrogram
plot(re.hclust, main="dist = euclidean, linkage = single", sub="",
     xlab="", ylab="dissimilarity",  
     hang=-1)
cutoff <- 1.05e+6
points(c(0, nrow(data_for_analysis)), c(cutoff, cutoff), type="l", col="red", lwd=2, lty=2)

# horizontal dendrogram
plot(as.dendrogram(re.hclust), horiz=T, 
     main="dist = euclidean, linkage = single", 
     ylab="", xlab="dissimilarity")
points(c(cutoff, cutoff), c(0, nrow(data_for_analysis)), 
       type="l", lwd=2, lty=2,
       col="red")





# average linkage
re.hclust <- hclust(d=distMatrix, method="average")

# vertical dendrogram
plot(re.hclust, 
     main="dist = euclidean, linkage = average", sub="", 
     xlab="", ylab="dissimilarity",
     hang=-1)
cutoff <- 1.75e+6
points(c(0, nrow(data_for_analysis)), c(cutoff, cutoff), type="l", col="red", lwd=2, lty=2)

plot(re.hclust, 
     main="dist = euclidean, linkage = average", sub="", 
     xlab="", ylab="dissimilarity")

# horizontal dendrogram
plot(as.dendrogram(re.hclust), horiz=T, 
     main="dist = euclidean, linkage = average", 
     ylab="", xlab="dissimilarity")
points(c(cutoff, cutoff), c(0, nrow(data_for_analysis)), 
       type="l", lwd=2, lty=2,
       col="red")





# centroid linkage
re.hclust <- hclust(d=distMatrix, method="centroid")

# vertical dendrogram
plot(re.hclust, main="dist = euclidean, linkage = centroid", sub="",
     xlab="", ylab="dissimilarity",
     hang=-1)
cutoff <- 8e+5
points(c(0, nrow(data_for_analysis)), c(cutoff, cutoff), 
       type="l", lwd=2, lty=2,
       col="red")

# horizontal dendrogram
plot(as.dendrogram(re.hclust), horiz=T, 
     main="dist = euclidean, linkage = centroid", 
     ylab="", xlab="disimilarity")
points(c(cutoff, cutoff), c(0, nrow(data_for_analysis)), 
       type="l", lwd=2, lty=2,
       col="red")





# manhattan dissimilarity
distMatrix <- dist(x=data_for_analysis, method="manhattan")

# complete linkage
re.hclust <- hclust(d=distMatrix, method="complete")

plot(as.dendrogram(re.hclust), horiz=T,
     main="dist = manhattan, linkage = complete",
     ylab="", xlab="dissimilarity")
cutoff <- 8e+6
points(c(cutoff, cutoff), c(0, nrow(data_for_analysis)),
       type="l", lwd=2, lty=2,
       col="red")


# single linkage
re.hclust <- hclust(d=distMatrix, method="single")

plot(as.dendrogram(re.hclust), horiz=T,
     main="dist = manhattan, linkage = single",
     ylab="", xlab="dissimilarity")



# average linkage
re.hclust <- hclust(d=distMatrix, method="average")

plot(as.dendrogram(re.hclust), horiz=T,
     main="dist = manhattan, linkage = average",
     ylab="", xlab="dissimilarity")




# centroid linkage
re.hclust <- hclust(d=distMatrix, method="centroid")

plot(as.dendrogram(re.hclust), horiz=T,
     main="dist = manhattan, linkage = centroid",
     ylab="", xlab="dissimilarity")









# =======================================
# Expectation maximization clustering
# =======================================

# 1. A 1-dim Gaussian mixture example

# 1.1 scenario 1
# We know 
# 1) data are from two sources, 
# 2) the two sources are gaussian, 
# 3) the source of each data point
#
# but we do not know
# 1) the parameters of the gaussian distributions
#
# Goal: to estimate the parameters of the gaussian distributions

# generate sample data from two gaussian distributions
mu_b <- 3
sigma_b <- 2

mu_r <- 20
sigma_r <- 2


N <- 10
x_b <- rnorm(n=N, mean=mu_b, sd=sigma_b)
x_r <- rnorm(n=N, mean=mu_r, sd=sigma_r)





# plot the sample data
plot(x_b, rep(0, times=length(x_b)),
     pch=16, cex=1,
     xlim=c(min(x_b, x_r), max(x_b, x_r)),
     xlab="x", ylab="")
points(x_r, rep(0, times=length(x_r)), 
       pch=16, cex=1,
       col="red")





# estimate the mean and standard deviation of the two gaussians
mu_b_estimate <- sum(x_b)/length(x_b)
sigma_b_estimate <- sqrt(sum((x_b - mu_b_estimate)^2)/(length(x_b)-1))

mu_r_estimate <- sum(x_r)/length(x_r)
sigma_r_estimate <- sqrt(sum((x_r - mu_r_estimate)^2)/(length(x_r)-1))





# plot the original PDF and the estimated PDF
x_b_generated <- seq(from=mu_b-5*sigma_b, to=mu_b+5*sigma_b, by=10*sigma_b/100)
y_b_generated <- 1/(sigma_b * sqrt(2*pi)) * exp(-0.5 * ((x_b_generated-mu_b)/sigma_b)^2)

x_r_generated <- seq(from=mu_r-5*sigma_r, to=mu_r+5*sigma_r, by=10*sigma_r/100)
y_r_generated <- 1/(sigma_r * sqrt(2*pi)) * exp(-0.5 * ((x_r_generated-mu_r)/sigma_r)^2)


x_b_from_estimatedPDF <- seq(from=mu_b_estimate-5*sigma_b_estimate, 
                             to=mu_b_estimate+5*sigma_b_estimate, by=10*sigma_b_estimate/100)
y_b_from_estimatedPDF <- 1/(sigma_b_estimate * sqrt(2*pi)) * exp(-0.5 * ((x_b_from_estimatedPDF-mu_b_estimate)/sigma_b_estimate)^2)


x_r_from_estimatedPDF <- seq(from=mu_r_estimate-5*sigma_r_estimate, 
                             to=mu_r_estimate+5*sigma_r_estimate, by=10*sigma_r_estimate/100)
y_r_from_estimatedPDF <- 1/(sigma_r_estimate * sqrt(2*pi)) * exp(-0.5 * ((x_r_from_estimatedPDF-mu_r_estimate)/sigma_r_estimate)^2)



plot(x_b_generated, y_b_generated,
     type="l", lwd=2,
     col="black",
     xlim=c(min(x_b_generated, x_r_generated), max(x_b_generated, x_r_generated)), 
     ylim=c(min(y_b_generated, y_r_generated), max(y_b_generated, y_r_generated)),
     xlab="x", ylab="f(x)",
     main="pdf of two Gaussian distributions")
points(x_r_generated, y_r_generated, 
       type="l", lwd=2,
       col="red")
points(x_b_from_estimatedPDF, y_b_from_estimatedPDF, 
       type="l", lty=3, lwd=2,
       col="black")
points(x_r_from_estimatedPDF, y_r_from_estimatedPDF, 
       type="l", lty=3, lwd=2,
       col="red")

points(x_b, rep(0, times=length(x_b)),
     pch=16, cex=1)
points(x_r, rep(0, times=length(x_r)), 
       pch=16, cex=1,
       col="red")






# 1.2 scamario 2
#
# We know
# 1) data are from two sources
# 2) the two sources are gaussian
# 3) the parameters of the two gaussian distribution
#
# but we do NOT know
# 1) the source of each data point
#
# Goal: to estimate the source of each data point

x <- c(x_b, x_r)

z <- matrix(0, nrow=length(x), ncol=2)



for (i in 1:length(x)) {
    p_xi_b <- 1/(sigma_b*sqrt(2*pi)) * exp(-0.5 * ((x[i]-mu_b)/sigma_b)^2)
    p_xi_r <- 1/(sigma_r*sqrt(2*pi)) * exp(-0.5 * ((x[i]-mu_r)/sigma_r)^2)
    
    z[i,1] <- p_xi_b * p_b / (p_xi_b * p_b + p_xi_r * p_r)
    z[i,2] <- p_xi_r * p_r / (p_xi_b * p_b + p_xi_r * p_r)
}





# 1.3 scanario 3
#
# We know
# 1) the data are from two sources
# 2) the two sources are gaussion distributions

# but do not know
# 1) the parameters of the two gaussians
# 2) the source of each data point

# Goal:
# 1) estimate the parameters of the two gaussians
# 2) the source of each data point





rm(list=ls())
graphics.off()





# 1.3.1 generate sample data

mu_b <- 3
sigma_b <- 3

mu_r <- 10
sigma_r <- 1


n_b <- 50
n_r <- 80

x_b <- rnorm(n=n_b, mean=mu_b, sd=sigma_b)
x_r <- rnorm(n=n_r, mean=mu_r, sd=sigma_r)

x_b_for_plot <- seq(from=mu_b-5*sigma_b, to=mu_b+5*sigma_b, by=10*sigma_b/100)
y_b_for_plot <- 1/(sigma_b * sqrt(2*pi)) * exp(-0.5 * ((x_b_for_plot-mu_b)/sigma_b)^2)

x_r_for_plot <- seq(from=mu_r-5*sigma_r, to=mu_r+5*sigma_r, by=10*sigma_r/100)
y_r_for_plot <- 1/(sigma_r * sqrt(2*pi)) * exp(-0.5 * ((x_r_for_plot-mu_r)/sigma_r)^2)

x <- c(x_b, x_r)



plot(x, rep(0, length(x)), pch=16, cex=1)
points(x_r, rep(0, length(x_r)), pch=16, cex=1, col="red")
points(x_b_for_plot, y_b_for_plot,
     type="l", lwd=2,
     col="black")
points(x_r_for_plot, y_r_for_plot,
       type="l", lwd=2,
       col="red")







# 1.3.2 initialize the paremeters for the two gaussians
pro_b_iter <- 0.5
pro_r_iter <- 0.5


mu_b_iter <- 2
sigma_b_iter <- 1

mu_r_iter <- 4
sigma_r_iter <- 1


# plot the two initial PDFs
x_b_iter <- seq(from=mu_b_iter-5*sigma_b_iter, to=mu_b_iter+5*sigma_b_iter, by=10*sigma_b_iter/100)
y_b_iter <- 1/(sigma_b_iter * sqrt(2*pi)) * exp(-0.5 * ((x_b_iter-mu_b_iter)/sigma_b_iter)^2)

x_r_iter <- seq(from=mu_r_iter-5*sigma_r_iter, to=mu_r_iter+5*sigma_r_iter, by=10*sigma_r_iter/100)
y_r_iter <- 1/(sigma_r_iter * sqrt(2*pi)) * exp(-0.5 * ((x_r_iter-mu_r_iter)/sigma_r_iter)^2)

points(x_b_iter, y_b_iter,
       type="l", lty=3, lwd=2,
       col="black")

points(x_r_iter, y_r_iter,
       type="l", lty=3, lwd=2,
       col="red")






# 1.3.3 First iteration

# E step: calculate the posterior probabilities for every data point

z <- matrix(0, nrow=length(x), ncol=2)


for (i in 1:length(x)) {
    p_xi_b <- 1/(sigma_b_iter*sqrt(2*pi)) * exp(-0.5 * ((x[i]-mu_b_iter)/sigma_b_iter)^2)
    p_xi_r <- 1/(sigma_r_iter*sqrt(2*pi)) * exp(-0.5 * ((x[i]-mu_r_iter)/sigma_r_iter)^2)
    
    z[i,1] <- p_xi_b * pro_b_iter / (p_xi_b * pro_b_iter + p_xi_r * pro_r_iter)
    
    # z[i,2] <- p_xi_r * p_r / (p_xi_b * p_b + p_xi_r * p_r)
    z[i, 2] <- 1 - z[i, 1]
}

II <- apply(z, MARGIN=1, which.max)
II_b <- which(II == 1)
II_r <- which(II == 2)






# M step: update the parameters of the two gaussian distributions

pro_b_iter <- sum(z[,1]) / length(x)
pro_r_iter <- sum(z[,2]) / length(x)

mu_b_iter <- sum(z[, 1] * x) / sum(z[, 1])
sigma_b_iter <- sqrt(sum(z[, 1] * (x - mu_b_iter)^2) / sum(z[, 1]))

mu_r_iter <- sum(z[, 2] * x) / sum(z[, 2])
sigma_r_iter <- sqrt(sum(z[, 2] * (x - mu_r_iter)^2) / sum(z[, 2]))


# plot the new PDFs
x_b_iter <- seq(from=mu_b_iter-5*sigma_b_iter, to=mu_b_iter+5*sigma_b_iter, by=10*sigma_b_iter/100)
y_b_iter <- 1/(sigma_b_iter * sqrt(2*pi)) * exp(-0.5 * ((x_b_iter-mu_b_iter)/sigma_b_iter)^2)

x_r_iter <- seq(from=mu_r_iter-5*sigma_r_iter, to=mu_r_iter+5*sigma_r_iter, by=10*sigma_r_iter/100)
y_r_iter <- 1/(sigma_r_iter * sqrt(2*pi)) * exp(-0.5 * ((x_r_iter-mu_r_iter)/sigma_r_iter)^2)

points(x_b_iter, y_b_iter,
       type="l", lty=2, lwd=2,
       col="black")

points(x_r_iter, y_r_iter,
       type="l", lty=2, lwd=2,
       col="red")







# 1.3.4 Second iteration

# E step
z <- matrix(0, nrow=length(x), ncol=2)


for (i in 1:length(x)) {
    p_xi_b <- 1/(sigma_b_iter*sqrt(2*pi)) * exp(-0.5 * ((x[i]-mu_b_iter)/sigma_b_iter)^2)
    p_xi_r <- 1/(sigma_r_iter*sqrt(2*pi)) * exp(-0.5 * ((x[i]-mu_r_iter)/sigma_r_iter)^2)
    
    z[i,1] <- p_xi_b * pro_b_iter / (p_xi_b * pro_b_iter + p_xi_r * pro_r_iter)
    # z[i,2] <- p_xi_r * pro_r_iter / (p_xi_b * pro_b_iter + p_xi_r * pro_r_iter)
    z[i,2] <- 1 - z[i,1]
}

II <- apply(z, MARGIN=1, which.max)
II_b <- which(II == 1)
II_r <- which(II == 2)







# M step
pro_b_iter <- sum(z[,1]) / length(x)
pro_r_iter <- 1 - pro_b_iter

mu_b_iter <- sum(z[, 1] * x) / sum(z[, 1])
sigma_b_iter <- sqrt(sum(z[, 1] * (x - mu_b_iter)^2) / sum(z[, 1]))

mu_r_iter <- sum(z[, 2] * x) / sum(z[, 2])
sigma_r_iter <- sqrt(sum(z[, 2] * (x - mu_r_iter)^2) / sum(z[, 2]))



# plot the new PDFs
x_b_iter <- seq(from=mu_b_iter-5*sigma_b_iter, to=mu_b_iter+5*sigma_b_iter, by=10*sigma_b_iter/100)
y_b_iter <- 1/(sigma_b_iter * sqrt(2*pi)) * exp(-0.5 * ((x_b_iter-mu_b_iter)/sigma_b_iter)^2)

x_r_iter <- seq(from=mu_r_iter-5*sigma_r_iter, to=mu_r_iter+5*sigma_r_iter, by=10*sigma_r_iter/100)
y_r_iter <- 1/(sigma_r_iter * sqrt(2*pi)) * exp(-0.5 * ((x_r_iter-mu_r_iter)/sigma_r_iter)^2)

points(x_b_iter, y_b_iter,
       type="l", lty=5, lwd=2,
       col="black")

points(x_r_iter, y_r_iter,
       type="l", lty=5, lwd=2,
       col="red")







# determine the initial parameters using hierarchical clustering
distMatrix <- dist(x, method="manhattan")
re.hclust <- hclust(distMatrix, method="complete")

plot(re.hclust, hang=-1)

memb <- cutree(re.hclust, k=2)
II_b <- which(memb==1)
II_r <- which(memb==2)

pro_b_iter <- length(II_b) / length(memb)
pro_r_iter <- 1 - pro_b_iter

mu_b_iter <- mean(x[II_b])
mu_r_iter <- mean(x[II_r])

sigma_b_iter <- sqrt(sum((x[II_b] - mu_b_iter)^2 ) / (length(II_b)-1) )
sigma_r_iter <- sqrt(sum((x[II_r] - mu_r_iter)^2 ) / (length(II_r)-1) )








# Now use a function for EM
source("/Users/xdu4/Documents/Duxiuxia/Bitbucket/machine_learning_tool_box/MLTB.R")

re.myEM <- d_EM(matrix(x, ncol=1), G=2)








# 2. A 2-dim Gaussian mixture example

packageName <- "mvtnorm"
if (!is.element(el=packageName, set=installed.packages(lib.loc=path_to_R_libs)[,1])) {
    install.packages(pkgs=packageName, 
                     lib=path_to_R_libs, 
                     repos="http://cran.us.r-project.org",
                     dependencies=T)
}
library(package=packageName,
        lib.loc=path_to_R_libs,
        character.only=T)





# 2.1 visualize data generated under different parameters

# uncorrelated attributes
n_b <- 100
mu_b <- c(3, 3)
SIGMA_b <- matrix(c(1, 0, 0, 1), nrow=2, ncol=2) # covariance matrix


n_r <- 100
mu_r <- c(10, 10)
SIGMA_r <- matrix(c(1, 0, 0, 1), nrow=2, ncol=2) # covariance matrix


x_b <- rmvnorm(n_b, mean=mu_b, sigma=SIGMA_b)
x_r <- rmvnorm(n_r, mean=mu_r, sigma=SIGMA_r)
x <- rbind(x_b, x_r)

plot(x,
     pch=16, cex=1)
points(x_r,
       pch=16, cex=1,
       col="red")




# change variance
SIGMA_b <- matrix(c(3, 0, 0, 1), nrow=2, ncol=2) # covariance matrix

x_b <- rmvnorm(n_b, mean=mu_b, sigma=SIGMA_b)
x_r <- rmvnorm(n_r, mean=mu_r, sigma=SIGMA_r)
x <- rbind(x_b, x_r)

plot(x,
     pch=16, cex=1)
points(x_r,
       pch=16, cex=1,
       col="red")







# correlated attributes: positively correlated
SIGMA_b <- matrix(c(1, 1, 1, 1), nrow=2, ncol=2) # covariance matrix

x_b <- rmvnorm(n_b, mean=mu_b, sigma=SIGMA_b)
x_r <- rmvnorm(n_r, mean=mu_r, sigma=SIGMA_r)
x <- rbind(x_b, x_r)

plot(x,
     pch=16, cex=1)
points(x_r,
       pch=16, cex=1,
       col="red")






SIGMA_b <- matrix(c(1, 1, 1, 2), nrow=2, ncol=2) # covariance matrix

x_b <- rmvnorm(n_b, mean=mu_b, sigma=SIGMA_b)
x_r <- rmvnorm(n_r, mean=mu_r, sigma=SIGMA_r)
x <- rbind(x_b, x_r)

plot(x,
     pch=16, cex=1)
points(x_r,
       pch=16, cex=1,
       col="red")






# correlated attributes: negatively correlated
SIGMA_b <- matrix(c(1, -1, -1, 1), nrow=2, ncol=2) # covariance matrix

x_b <- rmvnorm(n_b, mean=mu_b, sigma=SIGMA_b)
x_r <- rmvnorm(n_r, mean=mu_r, sigma=SIGMA_r)
x <- rbind(x_b, x_r)

plot(x,
     pch=16, cex=1)
points(x_r,
       pch=16, cex=1,
       col="red")






SIGMA_b <- matrix(c(1, -1, -1, 2), nrow=2, ncol=2) # covariance matrix

x_b <- rmvnorm(n_b, mean=mu_b, sigma=SIGMA_b)
x_r <- rmvnorm(n_r, mean=mu_r, sigma=SIGMA_r)
x <- rbind(x_b, x_r)

plot(x,
     pch=16, cex=1)
points(x_r,
       pch=16, cex=1,
       col="red")







# 2.2 generate sample data
n_b <- 100
mu_b <- c(7, 7)
# SIGMA_b <- matrix(c(1, 0, 0, 1), nrow=2, ncol=2) # covariance matrix
SIGMA_b <- matrix(c(1, 1, 1, 2), nrow=2, ncol=2) # covariance matrix



n_r <- 100
mu_r <- c(10, 10)
SIGMA_r <- matrix(c(1, 0, 0, 1), nrow=2, ncol=2) # covariance matrix


x_b <- rmvnorm(n_b, mean=mu_b, sigma=SIGMA_b)
x_r <- rmvnorm(n_r, mean=mu_r, sigma=SIGMA_r)
x <- rbind(x_b, x_r)

plot(x,
     pch=16, cex=1)
points(x_r,
       pch=16, cex=1,
       col="red")







# 2.3. hierarchical clustering to determine initial parameters of the two gaussians
distMatrix <- dist(x, method="euclidean")
re.hclust <- hclust(distMatrix, method="complete")

plot(re.hclust, hang=-1)

memb <- cutree(re.hclust, k=2)

II_b <- which(memb==1)
mu_b_iter <- colMeans(x[II_b, ])
SIGMA_b_iter <- cov(x[II_b, ])

II_r <- which(memb==2)
mu_r_iter <- colMeans(x[II_r, ])
SIGMA_r_iter <- cov(x[II_r, ])

pro_b_iter <- length(II_b) / nrow(x)
pro_r_iter <- 1 - pro_b_iter




# 2.4. First iteration

# E step
z <- matrix(0, nrow=nrow(x), ncol=2)


for (i in 1:nrow(x)) {
    p_xi_b <- 1/(sqrt(2 * pi * det(SIGMA_b_iter))) * 
        exp(-0.5 * ((x[i,]-mu_b_iter) %*% solve(SIGMA_b_iter) %*%  matrix(x[i,]-mu_b_iter, ncol=1)) ) 
    
    p_xi_r <- 1/(sqrt(2 * pi * det(SIGMA_r_iter))) * 
        exp(-0.5 * ((x[i,]-mu_r_iter) %*% solve(SIGMA_r_iter) %*%  matrix(x[i,]-mu_r_iter, ncol=1)) )
    
    p_b_xi <- (p_xi_b * pro_b_iter) / (p_xi_b*pro_b_iter + p_xi_r*pro_r_iter)
    p_r_xi <- 1 - p_b_xi
    
    z[i, ] <- c(p_b_xi, p_r_xi)
}

memb <- apply(z, MARGIN=1, which.max)






# M step
pro_b_iter <- sum(z[, 1]) / nrow(x)
pro_r_iter <- 1 - pro_b_iter



for (j in 1:ncol(x)) {
    mu_b_iter[j] <- sum(z[,1] * x[,j]) / sum(z[,1])
}

for (j in 1:ncol(x)) {
    mu_r_iter[j] <- sum(z[,2] * x[,j]) / sum(z[,2])
}



for (i in 1:ncol(x)) {
    for (j in 1:ncol(x)) {
        SIGMA_b_iter[i,j] <- sum(z[,1] * (x[,i]-mu_b_iter[i]) * (x[,j]-mu_b_iter[j]) ) / sum(z[,1])
    }
}



for (i in 1:ncol(x)) {
    for (j in 1:ncol(x)) {
        SIGMA_r_iter[i,j] <- sum(z[,2] * (x[,i]-mu_r_iter[i]) * (x[,j]-mu_r_iter[j]) ) / sum(z[,2])
    }
}





# 2.5. Second iteration

# E step
z <- matrix(0, nrow=nrow(x), ncol=2)


for (i in 1:nrow(x)) {
    p_xi_b <- 1/(sqrt(2 * pi * det(SIGMA_b_iter))) * 
        exp(-0.5 * ((x[i,]-mu_b_iter) %*% solve(SIGMA_b_iter) %*%  matrix(x[i,]-mu_b_iter, ncol=1)) ) 
    
    p_xi_r <- 1/(sqrt(2 * pi * det(SIGMA_r_iter))) * 
        exp(-0.5 * ((x[i,]-mu_r_iter) %*% solve(SIGMA_r_iter) %*%  matrix(x[i,]-mu_r_iter, ncol=1)) )
    
    p_b_xi <- (p_xi_b * pro_b_iter) / (p_xi_b*pro_b_iter + p_xi_r*pro_r_iter)
    p_r_xi <- 1 - p_b_xi
    
    z[i, ] <- c(p_b_xi, p_r_xi)
}

memb <- apply(z, MARGIN=1, which.max)






# M step
pro_b_iter <- sum(z[, 1]) / nrow(x)
pro_r_iter <- 1 - pro_b_iter



for (j in 1:ncol(x)) {
    mu_b_iter[j] <- sum(z[,1] * x[,j]) / sum(z[,1])
}

for (j in 1:ncol(x)) {
    mu_r_iter[j] <- sum(z[,2] * x[,j]) / sum(z[,2])
}



for (i in 1:ncol(x)) {
    for (j in 1:ncol(x)) {
        SIGMA_b_iter[i,j] <- sum(z[,1] * (x[,i]-mu_b_iter[i]) * (x[,j]-mu_b_iter[j]) ) / sum(z[,1])
    }
}



for (i in 1:ncol(x)) {
    for (j in 1:ncol(x)) {
        SIGMA_r_iter[i,j] <- sum(z[,2] * (x[,i]-mu_r_iter[i]) * (x[,j]-mu_r_iter[j]) ) / sum(z[,2])
    }
}











# 2.6 use R package "mclust" for EM
packageName <- "mclust"
if (!is.element(el=packageName, set=installed.packages(lib.loc=path_to_R_libs)[,1])) {
    install.packages(pkgs=packageName, 
                     lib=path_to_R_libs, 
                     repos="http://cran.us.r-project.org",
                     dependencies=T)
}
library(package=packageName,
        lib.loc=path_to_R_libs,
        character.only=T)





re.EM <- Mclust(x, G=2)
plot(re.EM, what=c("classification"))
plot(re.EM, what=c("density"))
plot(re.EM, what=c("uncertainty"))









# 3. Apply EM to real data

# raw data --> EM
re.EM <- Mclust(data_for_analysis, G=2)
re.EM$modelName
re.EM$classification
# SCLC_16HV_1, SCLC_16HV_2, SCLC_H524_1, SCLC_H524_2 are incorrectly clustered




# raw data --> PCA --> EM
re.PCA <- prcomp(x=data_for_analysis, scale=F)
re.PCA.EM <- Mclust(re.PCA$x, G=2)
re.PCA.EM$modelName
re.PCA.EM$classification
plot(re.PCA.EM, what=c("classification"))
# all objects are correctly clustered
# fewer parameters to estimate



# raw data --> PCA --> PC1 --> EM
plot(re.PCA$x[, 1], rep(0, times=nrow(data_for_analysis)), pch=16, cex=1)
points(re.PCA$x[21:40, 1], rep(0, times=20), pch=16, cex=1, col="red")
re.PCA.EM <- Mclust(re.PCA$x[, 1], G=2)
re.PCA.EM$modelName
re.PCA.EM$classification
# multiple SCLC objects are incorrectly clustered



# plot the PC1 together with the gaussian distribution
mu_1 <- re.PCA.EM$parameters$mean[1]
sigma_1 <- sqrt(re.PCA.EM$parameters$variance$sigmasq[1])
x1 <- seq(from=mu_1-5*sigma_1,
          to=mu_1+5*sigma_1,
          by=10*sigma_1/100)

y1 <- 1/(sigma_1*sqrt(2*pi)) * exp(-0.5 * ((x1-mu_1)/sigma_1)^2)


mu_2 <- re.PCA.EM$parameters$mean[2]
sigma_2 <- sqrt(re.PCA.EM$parameters$variance$sigmasq[2])
x2 <- seq(from=mu_2-5*sigma_2,
          to=mu_2+5*sigma_2,
          by=10*sigma_2/100)

y2 <- 1/(sigma_2*sqrt(2*pi)) * exp(-0.5 * ((x2-mu_2)/sigma_2)^2)


plot(re.PCA$x[,1], rep(0, times=length(re.PCA$x[,1])), pch=16, cex=1, 
     xlim=c(min(x1, x2), max(x1, x2)), ylim=c(min(y1, y2), max(y1, y2))) # PC1

points(re.PCA$x[21:40, 1], rep(0, times=length(re.PCA$x[21:40, 1])), 
       pch=16, cex=1, col="red")

II_1 <- which(re.PCA.EM$classification==1)
II_2 <- which(re.PCA.EM$classification==2)

points(re.PCA$x[II_1,1], rep(1e-7, times=length(II_1)),
       pch=2)
points(re.PCA$x[II_2,1], rep(1e-7, times=length(II_2)), 
       pch=2,
       col="red")

points(x1, y1, type="l", col="red") # estimated gaussian

points(x2, y2, type="l") # estimated gaussian

legend("topright", legend=c("true labels", "predicted labels"),
       pch=c(16, 2))
legend("topleft", legend="predicted distribution", lty=1)







# raw data --> PAM
re.pam <- pam(x=data_for_analysis, k=2, diss=F)
re.pam$clustering[1:20]
re.pam$clustering[21:40]
# NSCLC_H358_2, NSCLC_H522_1, NSCLC_H522_2, NSCLC_PC9_1 are incorrectly clustered


# raw data --> PCA --> PAM
re.PCA.pam <- pam(x=re.PCA$x, k=2, diss=F)
re.PCA.pam$clustering[1:20]
re.PCA.pam$clustering[21:40]
# NSCLC_H358_2, NSCLC_H522_1, NSCLC_H522_2, NSCLC_PC9_1 are incorrectly clustered


# raw data --> PCA --> PC1 --> PAM
re.PCA.pam <- pam(x=re.PCA$x[, 1], k=2, diss=F)
re.PCA.pam$clustering[1:20]
re.PCA.pam$clustering[21:40]
# perfect clustering








# raw data --> standardized data --> PCA --> EM
re.PCA <- prcomp(x=data_for_analysis, scale=T)
re.PCA.EM <- Mclust(data=re.PCA$x, G=2)
re.PCA.EM$modelName
re.PCA.EM$classification
# NSCLC_H522_1 is incorrectly clustered



# raw data --> standardized data --> PCA --> PC1 --> EM
re.PCA <- prcomp(x=data_for_analysis, scale=T)
plot(re.PCA$x[, 1], rep(0, times=nrow(re.PCA$x)), pch=16, cex=1)
points(re.PCA$x[21:40, 1], rep(0, times=20), col="red", pch=16, cex=1)
re.PCA.EM <- Mclust(data=re.PCA$x[,1], G=2)
re.PCA.EM$modelName
re.PCA.EM$classification
# NSCLC_H522_1, NSCLC_H522_2 are incorrectly clustered



# plot the PC1 together with the gaussian distribution
mu_1 <- re.PCA.EM$parameters$mean[1]
sigma_1 <- sqrt(re.PCA.EM$parameters$variance$sigmasq)
x1 <- seq(from=mu_1-5*sigma_1,
          to=mu_1+5*sigma_1,
          by=10*sigma_1/100)

y1 <- 1/(sigma_1*sqrt(2*pi)) * exp(-0.5 * ((x1-mu_1)/sigma_1)^2)


mu_2 <- re.PCA.EM$parameters$mean[2]
sigma_2 <- sqrt(re.PCA.EM$parameters$variance$sigmasq)
x2 <- seq(from=mu_2-5*sigma_2,
          to=mu_2+5*sigma_2,
          by=10*sigma_2/100)

y2 <- 1/(sigma_2*sqrt(2*pi)) * exp(-0.5 * ((x2-mu_2)/sigma_2)^2)


II_1 <- which(re.PCA.EM$classification==1)
II_2 <- which(re.PCA.EM$classification==2)

# true labels
plot(re.PCA$x[,1], rep(0, times=length(re.PCA$x[,1])), pch=16, cex=1) # PC1
points(re.PCA$x[21:40, 1], rep(0, times=length(re.PCA$x[21:40, 1])), 
       pch=16, cex=1, col="red") # PC1


# predicted labels
points(re.PCA$x[II_1,1], rep(0.1, times=length(re.PCA$x[II_1,1])), 
       pch=2,
       col="red") # PC1
points(re.PCA$x[II_2, 1], rep(0.1, times=length(re.PCA$x[II_2, 1])),
       pch=2) # PC1

points(x1, y1, type="l", col="red")
points(x2, y2, type="l")

legend("topright", legend=c("true labels", "predicted labels"),
       pch=c(16, 2))
legend("topleft", legend="predicted distribution", lty=1)





# =======================================
# Effect of scaling on PCA and clustering
# =======================================

packageName <- "mclust"
if (!is.element(el=packageName, set=installed.packages(lib.loc=path_to_R_libs)[,1])) {
    install.packages(pkgs=packageName, 
                     lib=path_to_R_libs, 
                     repos="http://cran.us.r-project.org",
                     dependencies=T)
}
library(package=packageName,
        lib.loc=path_to_R_libs,
        character.only=T)





packageName <- "cluster"
if (!is.element(el=packageName, set=installed.packages(lib.loc=path_to_R_libs)[,1])) {
    install.packages(pkgs=packageName, 
                     lib=path_to_R_libs, 
                     repos="http://cran.us.r-project.org",
                     dependencies=T)
}
library(package=packageName,
        lib.loc=path_to_R_libs,
        character.only=T)







# 1. raw data --> PCA
re.PCA <- prcomp(x=data_for_analysis, scale=F)


# plot PC1 scores
plot(re.PCA$x[,1], rep(0, times=length(re.PCA$x[,1])),
     pch=16, cex=1,
     xlab="PCA score", ylab="",
     main="PC1 without standardization of variables")
points(re.PCA$x[21:40, 1], rep(0, times=length(re.PCA$x[21:40, 1])),
       pch=16, cex=1,
       col="red")
legend("topright", legend=c("NSCLC", "SCLC"), pch=c(16, 16), col=c("black", "red") )



# plot loadings
plot(re.PCA$rotation[,1], re.PCA$rotation[,2],
     pch=16, cex=1,
     xlab="PC1", ylab="PC2",
     main="loadings")

text(re.PCA$rotation[,1], re.PCA$rotation[,2], labels=colnames(data_for_analysis), pos=3)

II <- which(colnames(data_for_analysis)=="1381")


# raw data --> PCA --> EM
re.PCA.EM <- Mclust(re.PCA$x, G=2)
re.PCA.EM$classification # perfect clustering



# raw data --> PAM
re.pam <- pam(x=data_for_analysis, k=2, diss=F)
re.pam$clustering[1:20]
re.pam$clustering[21:40]
# NSCLC_H358_2, NSCLC_H522_1, NSCLC_H522_2, NSCLC_PC9_1 are wrongly clustered





# 2. scale one attribute
scaling_factor <- 10


# scale the attribute with the largest loading for PC1
new_data_1 <- cbind(data_for_analysis[,1:II-1], 
                  data_for_analysis[,II]/scaling_factor, 
                  data_for_analysis[,(II+1):ncol(data_for_analysis)] )

colnames(new_data_1) <- colnames(data_for_analysis)


# plot PC1
re.PCA <- prcomp(x=new_data_1, scale=F)
plot(re.PCA$x[,1], rep(0, times=length(re.PCA$x[,1])),
     pch=16, cex=1)
points(re.PCA$x[21:40, 1], rep(0, times=length(re.PCA$x[21:40, 1])),
       pch=16, cex=1,
       col="red")

# plot loadings
plot(re.PCA$rotation[,1], re.PCA$rotation[,2],
     pch=16, cex=1)

text(re.PCA$rotation[,1], re.PCA$rotation[,2], labels=colnames(data_for_analysis), pos=3)

# raw data --> scale one attribute --> PCA --> EM
re.PCA.EM <- Mclust(re.PCA$x, G=2)
re.PCA.EM$classification
# perfect clustering






# raw data --> scale one attribute --> PAM
re.pam <- pam(x=new_data_1, k=2, diss=F)
re.pam$clustering[1:20]
re.pam$clustering[21:40]
# NSCLC_H522_1, NSCLC_H358_2, NSCLC_H522_2, NSCLC_PC9_1, NSCLC_PC9_2 are wrongly clustered





# 3. scale a second attribute
II <- which(colnames(new_data_1)=="1076")

new_data_2 <- cbind(new_data_1[,1:II-1], 
                  new_data_1[,II]/scaling_factor, 
                  new_data_1[,(II+1):ncol(new_data_1)] )

colnames(new_data_2) <- colnames(data_for_analysis)

re.PCA <- prcomp(x=new_data_2, scale=F)

# plot PC1 scores
PC_id <- 1

plot(re.PCA$x[,PC_id], rep(0, times=length(re.PCA$x[,PC_id])),
     pch=16, cex=1)
points(re.PCA$x[21:40, PC_id], rep(0, times=length(re.PCA$x[21:40, PC_id])),
       pch=16, cex=1,
       col="red")

# plot PC2 scores
PC_id <- 2
plot(re.PCA$x[,PC_id], rep(0, times=length(re.PCA$x[,PC_id])),
     pch=16, cex=1)
points(re.PCA$x[21:40, PC_id], rep(0, times=length(re.PCA$x[21:40, PC_id])),
       pch=16, cex=1,
       col="red")


# plot loadings
plot(re.PCA$rotation[,1], re.PCA$rotation[,2],
     pch=16, cex=1)

text(re.PCA$rotation[,1], re.PCA$rotation[,2], labels=colnames(data_for_analysis), pos=3)



# raw data --> scaling two attributes --> PCA --> EM
re.PCA.EM <- Mclust(re.PCA$x, G=2)
re.PCA.EM$classification # got stuck




# raw data --> scaling two attributes --> PAM
re.pam <- pam(x=new_data_2, k=2, diss=F)
re.pam$clustering[1:20]
re.pam$clustering[21:40]
# NSCLC_H2228_1, NSCLC_H2228_2, NSCLC_H358_2, NSCLC_H522_1, NSCLC_H522_2, 
# NSCLC_PC9_1, NSCLC_PC9_2, SCLC_16HV_1, SCLC_16HV_2 are wrongly clustered










# packageName <- "clue"
# if (!is.element(el=packageName, set=installed.packages(lib.loc=path_to_R_libs)[,1])) {
#     install.packages(pkgs=packageName, 
#                      lib=path_to_R_libs, 
#                      repos="http://cran.us.r-project.org",
#                      dependencies=T)
# }
# library(package=packageName,
#         lib.loc=path_to_R_libs,
#         character.only=T)
# 
# 
# packageName <- "lpSolve"
# if (!is.element(el=packageName, set=installed.packages(lib.loc=path_to_R_libs)[,1])) {
#     install.packages(pkgs=packageName, 
#                      lib=path_to_R_libs, 
#                      repos="http://cran.us.r-project.org",
#                      dependencies=T)
# }
# library(package=packageName,
#         lib.loc=path_to_R_libs,
#         character.only=T)
