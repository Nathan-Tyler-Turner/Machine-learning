#LDA

# Author: Xiuxia Du and Tyler Turner








MLTB_dir <- "C:/Users/Tyler/Desktop/R codes/MLTB" 
data_dir <- "C:/Users/Tyler/Documents"

setwd(data_dir)







# -----------------------------------------------------------------------------
# 1. Two-class LDA: apply LDA to a 2-dim toy dataset
# -----------------------------------------------------------------------------
# 1.1 the data

rm(list=ls())
graphics.off()





c_1 <- matrix(c(4, 1, 2, 4, 2, 3, 3, 6, 4, 4), nrow=5, byrow=TRUE)
c_2 <- matrix(c(9, 10, 6, 8, 9, 5, 8, 7, 10, 8), nrow=5, byrow=TRUE)
c_1
c_2

plot(c_1[,1], c_1[, 2],
     col="blue",
     pch=16, cex=1,
     asp=1,
     xlim=c(0,12), ylim=c(0,12),
     xlab="x1", ylab="x2",
     main="LDA")
points(c_2[,1], c_2[,2],
       col="red",
       pch=16, cex=1)





# 1.2 compute mu_1 and mu_2
mu_1 <- colMeans(c_1)
mu_2 <- colMeans(c_2)
mu_1
mu_2

points(mu_1[1], mu_1[2],
       pch=2, cex=1,
       col="blue")
points(mu_2[1], mu_2[2],
       pch=2, cex=1,
       col="red")





# 1.3 compute S_within
S_within_1 <- matrix(0, nrow=2, ncol=2)
S_within_2 <- matrix(0, nrow=2, ncol=2)


for (i in 1:nrow(c_1)) {
    S_within_1 <- S_within_1 + matrix((c_1[i,]-mu_1), nrow=2) %*% (c_1[i,]-mu_1)
}

for (i in 1:nrow(c_2)) {
    S_within_2 <- S_within_2 + matrix((c_2[i,]-mu_2), nrow=2) %*% (c_2[i,]-mu_2)
}

S_within <- S_within_1 + S_within_2
S_within




# 1.4 compute optimal W
W <- solve(S_within) %*% matrix((mu_1-mu_2), nrow=2)





# 1.5 normalize W
W <- W / sqrt(sum(W^2))





# 1.6 plot W
k <- 12 / W[1]
points(c(0, k*W[1]), c(0, k*W[2]), type="l", lwd=2, col="green")





# 1.7 compute and plot projections
projection_1 <- matrix(W, nrow=1) %*% t(c_1)
projection_2 <- matrix(W, nrow=1) %*% t(c_2)

theta <- atan(W[2] / W[1])



points(-c(projection_1, projection_2)*cos(theta), -c(projection_1, projection_2)*sin(theta),
       pch=0, cex=1,
       col="blue")
points(-projection_2*cos(theta), -projection_2*sin(theta),
       pch=0, cex=1,
       col="red")





# 1.8 examine W and the two variables
# plot W
plot(1:length(W), W, pch=16, cex=1,  xlim=c(0, length(W)), ylim=c(min(W), 1))
points(c(0, length(W)), c(0, 0), type="l")
text(1:length(W), W, labels=1:length(W), pos=3)



# plot the two variables
# points along the lower line are the first variable
# points along the upper line are the second variable 
c <- rbind(c_1, c_2)

offset <- 0.1
plot(c_1[,1], rep((1-offset), length(c_1[,1])), 
     pch=16, cex=1, 
     col="blue",
     xlim=c(min(c), max(c)), 
     ylim=c(0, 3))
points(c_2[,1], rep((1+offset), length(c_2[,1])),
       pch=16, cex=1,
       col="red")
points(c(0, max(c)), c(1, 1), type="l")

points(c_1[,2], rep((2-offset), length(c_1[,2])), 
       pch=16, cex=1, 
       col="blue")
points(c_2[,2], rep((2+offset), length(c_2[,2])),
       pch=16, cex=1,
       col="red")
points(c(0, max(c)), c(2, 2), type="l")




# -----------------------------------------------------------------------------
# 2. Two-class LDA: apply LDA to the 4-dim iris dataset
# -----------------------------------------------------------------------------
# 2.1 visualize the data
# plot the histogram of each variable

rm(list=ls())
graphics.off()



II_setosa <- which(iris$Species=="setosa")
II_versicolor <- which(iris$Species=="versicolor")
II_virginica <- which(iris$Species=="virginica")





# sepal length
variable_name <- "Sepal.Length"

break_points <- seq(from=min(iris[, variable_name]),
                    to=max(iris[, variable_name]),
                    by=(max(iris[, variable_name])-min(iris[, variable_name]))/20)

hist_sepal_length_setosa <- hist(iris[II_setosa, variable_name], breaks=break_points, plot=F)
hist_sepal_length_versicolor <- hist(iris[II_versicolor, variable_name], breaks=break_points, plot=F)
hist_sepal_length_virginica <- hist(iris[II_virginica, variable_name], breaks=break_points, plot=F)

plot(hist_sepal_length_setosa, 
     col="red",
     xlab="sepal length",
     main="Histogram of sepal length")

plot(hist_sepal_length_versicolor, col="green", add=T)

plot(hist_sepal_length_virginica, col="blue", add=T)

legend("topright", 
       legend=c("setosa", "versicolor", "virginica"), 
       pch=c(15, 15, 15),
       col=c("red", "green", "blue"))





# sepal width
variable_name <- "Sepal.Width"

break_points <- seq(from=min(iris[, variable_name]),
                    to=max(iris[, variable_name]),
                    by=(max(iris[, variable_name])-min(iris[, variable_name]))/20)

hist_sepal_length_setosa <- hist(iris[II_setosa, variable_name], breaks=break_points, plot=F)
hist_sepal_length_versicolor <- hist(iris[II_versicolor, variable_name], breaks=break_points, plot=F)
hist_sepal_length_virginica <- hist(iris[II_virginica, variable_name], breaks=break_points, plot=F)

plot(hist_sepal_length_setosa, 
     col="red",
     xlab="sepal width",
     main="Histogram of sepal width")

plot(hist_sepal_length_versicolor, col="green", add=T)

plot(hist_sepal_length_virginica, col="blue", add=T)

legend("topright", 
       legend=c("setosa", "versicolor", "virginica"), 
       pch=c(15, 15, 15),
       col=c("red", "green", "blue"))




# petal length
variable_name <- "Petal.Length"

break_points <- seq(from=min(iris[, variable_name]),
                    to=max(iris[, variable_name]),
                    by=(max(iris[, variable_name])-min(iris[, variable_name]))/20)

hist_sepal_length_setosa <- hist(iris[II_setosa, variable_name], breaks=break_points, plot=F)
hist_sepal_length_versicolor <- hist(iris[II_versicolor, variable_name], breaks=break_points, plot=F)
hist_sepal_length_virginica <- hist(iris[II_virginica, variable_name], breaks=break_points, plot=F)

plot(hist_sepal_length_setosa, 
     col="red",
     xlab="petal length",
     main="Histogram of petal length")

plot(hist_sepal_length_versicolor, col="green", add=T)

plot(hist_sepal_length_virginica, col="blue", add=T)

legend("topright", 
       legend=c("setosa", "versicolor", "virginica"), 
       pch=c(15, 15, 15),
       col=c("red", "green", "blue"))





# petal width
variable_name <- "Petal.Width"

break_points <- seq(from=min(iris[, variable_name]),
                    to=max(iris[, variable_name]),
                    by=(max(iris[, variable_name])-min(iris[, variable_name]))/20)

hist_sepal_length_setosa <- hist(iris[II_setosa, variable_name], breaks=break_points, plot=F)
hist_sepal_length_versicolor <- hist(iris[II_versicolor, variable_name], breaks=break_points, plot=F)
hist_sepal_length_virginica <- hist(iris[II_virginica, variable_name], breaks=break_points, plot=F)

plot(hist_sepal_length_setosa, 
     col="red",
     xlab="petal width",
     main="Histogram of petal width")

plot(hist_sepal_length_versicolor, col="green", add=T)

plot(hist_sepal_length_virginica, col="blue", add=T)

legend("topright", 
       legend=c("setosa", "versicolor", "virginica"), 
       pch=c(15, 15, 15),
       col=c("red", "green", "blue"))
# peta length and petal width are more discriminative than sepal data.





# 2.2 apply LDA to setosa and virginica data
c_1 <- as.matrix(iris[II_setosa, 1:4])
c_2 <- as.matrix(iris[II_virginica, 1:4])



# LDA
source(paste(MLTB_dir, "d_LDA.R", sep=.Platform$file.sep))
W <- d_LDA(x1=c_1, x2=c_2)




# normalize W
W <- W / sqrt(sum(W^2))
# W=[0.28, 0.22, -0.66, -0.66]





# plot projections onto W
projection_1 <- matrix(W, nrow=1) %*% t(c_1)
projection_2 <- matrix(W, nrow=1) %*% t(c_2)

plot(c(projection_1, projection_2), rep(0, times=length(projection_1)+length(projection_2)),
     pch=16, cex=1,
     col="red",
     xlab="projection onto W", ylab="",
     main="LDA")
points(projection_2, rep(0, times=length(projection_2)),
       pch=16, cex=1,
       col="blue")
legend("topright", legend=c("setosa", "virginica"), pch=c(16, 16), col=c("red", "blue"))





# examine W and the four variables
# plot W
plot(1:length(W), W, 
     pch=16, cex=1,  
     xlim=c(0, length(W)), ylim=c(min(W), max(W)))
points(c(0, length(W)), c(0, 0), type="l")
text(1:length(W), W, labels=1:length(W), pos=3)



# plot the four variables
c <- rbind(iris[II_setosa, 1:4], iris[II_virginica, 1:4])

plot(iris[II_setosa,1], rep(1-0.1, length(iris[II_setosa,1])), 
     pch=16, cex=1, 
     col="blue",
     xlim=c(min(c), max(c)), 
     ylim=c(0, 4),
     xlab="variable values", ylab="variable index",
     main="variables")
points(iris[II_virginica,1], rep(1+0.1, length(iris[II_virginica,1])),
       pch=16, cex=1,
       col="red")
points(c(0, max(c)), c(1, 1), type="l")

for (i in 2:4) {
    points(iris[II_setosa,i], rep(i-0.1, length(iris[II_setosa,i])), 
           pch=16, cex=1, 
           col="blue")
    points(iris[II_virginica,i], rep(i+0.1, length(iris[II_virginica,i])),
           pch=16, cex=1,
           col="red")
    points(c(0, max(c)), c(i, i), type="l")
}
legend("topright", legend=c("setosa", "virginica"), pch=c(16, 16), col=c("blue", "red"))





# 2.3 apply LDA to standardized iris
source(paste(MLTB_dir, "d_standardize.R", sep=.Platform$file.sep))
iris_standardized <- d_standardize(iris[, 1:4])


c_1 <- as.matrix(iris_standardized[II_setosa, 1:4])
c_2 <- as.matrix(iris_standardized[II_virginica, 1:4])


W <- d_LDA(x1=c_1, x2=c_2)




# normalize W
W <- W / sqrt(sum(W^2))






# plot projections onto W
projection_1 <- matrix(W, nrow=1) %*% t(c_1)
projection_2 <- matrix(W, nrow=1) %*% t(c_2)

plot(c(projection_1, projection_2), rep(0, times=length(projection_1)+length(projection_2)),
     pch=16, cex=1,
     col="red",
     xlab="projection onto W", ylab="",
     main="LDA")
points(projection_2, rep(0, times=length(projection_2)),
       pch=16, cex=1,
       col="blue")
legend("topright", legend=c("setosa", "virginica"), pch=c(16, 16), col=c("red", "blue"))





# examine W and the four variables
# plot W
plot(1:length(W), W, 
     pch=16, cex=1,  
     xlim=c(0, length(W)), ylim=c(min(W), max(W)))
points(c(0, length(W)), c(0, 0), type="l")
text(1:length(W), W, labels=1:length(W), pos=3)



# plot the four variables
c <- rbind(iris_standardized[II_setosa,1:4], iris_standardized[II_virginica,1:4])

plot(iris_standardized[II_setosa,1], rep(1-0.1, length(II_setosa)), 
     pch=16, cex=1, 
     col="blue",
     xlim=c(min(c), max(c)), 
     ylim=c(0, 4),
     xlab="variable values", ylab="variable index",
     main="variables")
points(iris_standardized[II_virginica,1], rep(1+0.1, length(iris_standardized[II_virginica,1])),
       pch=16, cex=1,
       col="red")
points(c(0, max(c)), c(1, 1), type="l")
for (i in 2:4) {
    points(iris_standardized[II_setosa,i], rep(i-0.1, length(II_setosa)), 
           pch=16, cex=1, 
           col="blue")
    points(iris_standardized[II_virginica,i], rep(i+0.1, length(iris_standardized[II_virginica,i])),
           pch=16, cex=1,
           col="red")
    points(c(0, max(c)), c(i, i), type="l")
}
legend("topright", legend=c("setosa", "virginica"), pch=c(16, 16), col=c("blue", "red"))







# 2.4 manipulate the virginica data
II <- which(iris_standardized[II_virginica, 1] < 1)
iris_standardized[II_virginica[II], 1] <- 1


c_1 <- as.matrix(iris_standardized[II_setosa, 1:4])
c_2 <- as.matrix(iris_standardized[II_virginica, 1:4])


W <- d_LDA(x1=c_1, x2=c_2)




# normalize W
W <- W / sqrt(sum(W^2))






# plot projections onto W
projection_1 <- matrix(W, nrow=1) %*% t(c_1)
projection_2 <- matrix(W, nrow=1) %*% t(c_2)

plot(c(projection_1, projection_2), rep(0, times=length(projection_1)+length(projection_2)),
     pch=16, cex=1,
     col="red",
     xlab="projection onto W", ylab="",
     main="LDA")
points(projection_2, rep(0, times=length(projection_2)),
       pch=16, cex=1,
       col="blue")
legend("topright", legend=c("setosa", "virginica"), pch=c(16, 16), col=c("red", "blue"))





# examine W and the four variables
# plot W
plot(1:length(W), W, 
     pch=16, cex=1,  
     xlim=c(0, length(W)), ylim=c(min(W), max(W)))
points(c(0, length(W)), c(0, 0), type="l")
text(1:length(W), W, labels=1:length(W), pos=3)



# plot the four variables
c <- rbind(iris_standardized[II_setosa,1:4], iris_standardized[II_virginica,1:4])

plot(iris_standardized[II_setosa,1], rep(1-0.1, length(II_setosa)), 
     pch=16, cex=1, 
     col="blue",
     xlim=c(min(c), max(c)), 
     ylim=c(0, 4),
     xlab="variable values", ylab="variable index",
     main="variables")
points(iris_standardized[II_virginica,1], rep(1+0.1, length(iris_standardized[II_virginica,1])),
       pch=16, cex=1,
       col="red")
for (i in 2:4) {
    points(iris_standardized[II_setosa,i], rep(i-0.1, length(II_setosa)), 
           pch=16, cex=1, 
           col="blue")
    points(iris_standardized[II_virginica,i], rep(i+0.1, length(iris_standardized[II_virginica,i])),
           pch=16, cex=1,
           col="red")
}
legend("topright", legend=c("setosa", "virginica"), pch=c(16, 16), col=c("blue", "red"))





# change petal length sign
iris_new <- cbind(iris[,1:2], -iris[,3], iris[,4])



c_1 <- as.matrix(iris_new[II_setosa, 1:4])
c_2 <- as.matrix(iris_new[II_virginica, 1:4])


W <- d_LDA(x1=c_1, x2=c_2)




# normalize W
W <- W / sqrt(sum(W^2))






# plot projections onto W
projection_1 <- matrix(W, nrow=1) %*% t(c_1)
projection_2 <- matrix(W, nrow=1) %*% t(c_2)

plot(c(projection_1, projection_2), rep(0, times=length(projection_1)+length(projection_2)),
     pch=16, cex=1,
     col="red",
     xlab="projection onto W", ylab="",
     main="LDA")
points(projection_2, rep(0, times=length(projection_2)),
       pch=16, cex=1,
       col="blue")
legend("topright", legend=c("setosa", "virginica"), pch=c(16, 16), col=c("red", "blue"))





# examine W and the four variables
# plot W
plot(1:length(W), W, 
     pch=16, cex=1,  
     xlim=c(0, length(W)), ylim=c(min(W), max(W)))
points(c(0, length(W)), c(0, 0), type="l")
text(1:length(W), W, labels=1:length(W), pos=3)



# plot the four variables
c <- rbind(iris_new[II_setosa,1:4], iris_new[II_virginica,1:4])

plot(iris_new[II_setosa,1], rep(1-0.1, length(II_setosa)), 
     pch=16, cex=1, 
     col="blue",
     xlim=c(min(c), max(c)), 
     ylim=c(0, 4),
     xlab="variable values", ylab="variable index",
     main="variables")
points(iris_new[II_virginica,1], rep(1+0.1, length(iris_new[II_virginica,1])),
       pch=16, cex=1,
       col="red")
for (i in 2:4) {
    points(iris_new[II_setosa,i], rep(i-0.1, length(II_setosa)), 
           pch=16, cex=1, 
           col="blue")
    points(iris_new[II_virginica,i], rep(i+0.1, length(iris_new[II_virginica,i])),
           pch=16, cex=1,
           col="red")
}
legend("topright", legend=c("setosa", "virginica"), pch=c(16, 16), col=c("blue", "red"))










# 2.5 apply LDA to setosa and versicolor data
c_1 <- as.matrix(iris[II_setosa, 1:4])
c_2 <- as.matrix(iris[II_versicolor, 1:4])


source(paste(MLTB_dir, "d_LDA.R", sep=.Platform$file.sep))
W <- d_LDA(x1=c_1, x2=c_2)





# normalize W
W <- W / sqrt(sum(W^2))
# W=[0.07, 0.43, -0.52, -0.73]





# plot projections onto W
projection_1 <- matrix(W, nrow=1) %*% t(c_1)
projection_2 <- matrix(W, nrow=1) %*% t(c_2)

plot(c(projection_1, projection_2), rep(0, times=length(projection_1)+length(projection_2)),
     pch=16, cex=1,
     col="red",
     xlab="projection onto W", ylab="",
     main="LDA")
points(projection_2, rep(0, times=length(projection_2)),
       pch=16, cex=1,
       col="blue")
legend("topright", legend=c("setosa", "versicolor"), pch=c(16, 16), col=c("red", "blue"))





# examine W and the four variables
# plot W
plot(1:length(W), W, 
     pch=16, cex=1,  
     xlim=c(0, length(W)), ylim=c(min(W), max(W)))
points(c(0, length(W)), c(0, 0), type="l")
text(1:length(W), W, labels=1:length(W), pos=3)



# plot the four variables
c <- rbind(iris[II_setosa,1:4], iris[II_versicolor,1:4])

plot(iris[II_setosa,1], rep(1-0.1, length(II_setosa)), 
     pch=16, cex=1, 
     col="blue",
     xlim=c(min(c), max(c)), 
     ylim=c(0, 4),
     xlab="variable values", ylab="variable index",
     main="variables")
points(iris[II_versicolor,1], rep(1+0.1, length(iris[II_versicolor,1])),
       pch=16, cex=1,
       col="red")
points(c(0, max(c)), c(1, 1), type="l")
for (i in 2:4) {
    points(iris[II_setosa,i], rep(i-0.1, length(II_setosa)), 
           pch=16, cex=1, 
           col="blue")
    points(iris[II_versicolor,i], rep(i+0.1, length(iris[II_versicolor,i])),
           pch=16, cex=1,
           col="red")
    points(c(0, max(c)), c(i, i), type="l")
}
legend("topright", legend=c("setosa", "versicolor"), pch=c(16, 16), col=c("blue", "red"))





# 2.6 apply LDA to versicolor data and virginica data
c_1 <- as.matrix(iris[II_virginica, 1:4])
c_2 <- as.matrix(iris[II_versicolor, 1:4])


W <- d_LDA(x1=c_1, x2=c_2)




# normalize W
W <- W / sqrt(sum(W^2))
# W=[-0.23, -0.35, 0.44, 0.79]





# plot projections onto W
projection_1 <- matrix(W, nrow=1) %*% t(c_1)
projection_2 <- matrix(W, nrow=1) %*% t(c_2)

plot(c(projection_1, projection_2), rep(0, times=length(projection_1)+length(projection_2)),
     pch=16, cex=1,
     col="blue",
     xlab="projection onto W", ylab="",
     main="LDA")
points(projection_2, rep(0, times=length(projection_2)),
       pch=16, cex=1,
       col="red")
legend("topright", legend=c("virginica", "versicolor"), pch=c(16, 16), col=c("blue", "red"))





# examine W and the four variables
# plot W
plot(1:length(W), W, 
     pch=16, cex=1,  
     xlim=c(0, length(W)), ylim=c(min(W), max(W)))
points(c(0, length(W)), c(0, 0), type="l")
text(1:length(W), W, labels=1:length(W), pos=3)



# plot the four variables
c <- rbind(iris[II_virginica,1:4], iris[II_versicolor,1:4])

plot(iris[II_virginica,1], rep(1-0.1, length(II_virginica)), 
     pch=16, cex=1, 
     col="blue",
     xlim=c(min(c), max(c)), 
     ylim=c(0, 4),
     xlab="variable values", ylab="variable index",
     main="variables")
points(iris[II_versicolor,1], rep(1+0.1, length(iris[II_versicolor,1])),
       pch=16, cex=1,
       col="red")
points(c(0, max(c)), c(1, 1), type="l")
for (i in 2:4) {
    points(iris[II_virginica,i], rep(i-0.1, length(II_virginica)), 
           pch=16, cex=1, 
           col="blue")
    points(iris[II_versicolor,i], rep(i+0.1, length(iris[II_versicolor,i])),
           pch=16, cex=1,
           col="red")
    points(c(0, max(c)), c(i, i), type="l")
}
legend("topright", legend=c("virginica", "versicolor"), pch=c(16, 16), col=c("blue", "red"))





# -----------------------------------------------------------------------------
# 3. multiple-class LDA: apply LDA to the three-class iris data
# -----------------------------------------------------------------------------


rm(list=ls())
graphics.off()



II_setosa <- which(iris$Species=="setosa")
II_versicolor <- which(iris$Species=="versicolor")
II_virginica <- which(iris$Species=="virginica")


# 3.1 compute the 4-dim mean vectors
mu_setosa <- colMeans(iris[II_setosa, 1:4])
mu_versicolor <- colMeans(iris[II_versicolor, 1:4])
mu_virginica <- colMeans(iris[II_virginica, 1:4])




# 3.2 compute S_within
S_within_setosa <- matrix(0, nrow=4, ncol=4)
S_within_versicolor <- matrix(0, nrow=4, ncol=4)
S_within_virginica <- matrix(0, nrow=4, ncol=4)

S_within <- matrix(0, nrow=4, ncol=4)


for (i in 1:length(II_setosa)) {
    current_object <- as.numeric(iris[II_setosa[i], 1:4])
    S_within_setosa <- S_within_setosa + matrix(current_object-mu_setosa, nrow=4) %*% (current_object-mu_setosa)
}


for (i in 1:length(II_versicolor)) {
    current_object <- as.numeric(iris[II_versicolor[i], 1:4])
    S_within_versicolor <- S_within_versicolor + matrix(current_object-mu_versicolor, nrow=4) %*% (current_object-mu_versicolor)
}


for (i in 1:length(II_virginica)) {
    current_object <- as.numeric(iris[II_virginica[i], 1:4])
    S_within_virginica <- S_within_virginica + matrix(current_object-mu_virginica, nrow=4) %*% (current_object-mu_virginica)
}

S_within <- S_within_setosa + S_within_versicolor + S_within_virginica





# 3.3 compute S_between
mu <- colMeans(iris[, 1:4])

S_between <- length(II_setosa) * ( matrix(mu_setosa-mu, nrow=4) %*% (mu_setosa-mu) ) + 
    length(II_versicolor) * ( matrix(mu_versicolor-mu, nrow=4) %*% (mu_versicolor-mu) ) + 
    length(II_virginica) * ( matrix(mu_virginica-mu, nrow=4) %*% (mu_virginica-mu) )





# 3.4 compute the eigenvalues and eigenvectors of ( solve(S_within) %*% S_between) and get W
re <- eigen(solve(S_within) %*% S_between)

W <- re$vectors[, 1:2]





# 3.5 transform the iris data into the new subspace
Y <- t(W) %*% t(iris[, 1:4])
Y <- t(Y)

plot(Y[II_setosa,1], Y[II_setosa,2],
     pch=16, cex=1,
     xlim=c(min(Y[,1]), max(Y[, 1])),
     ylim=c(min(Y[,2]), max(Y[,2])),
     col="red",
     xlab="LD1", ylab="LD2",
     main="LDA: iris projection onto the first two linear discrimants")

points(Y[II_versicolor, 1], Y[II_versicolor, 2],
       pch=16, cex=1,
       col="green")

points(Y[II_virginica, 1], Y[II_virginica, 2],
       pch=16, cex=1,
       col="blue")

legend("topright", legend=c("setosa", "versicolor", "virginica"), 
       pch=c(16, 16, 16), 
       col=c("red", "green", "blue"))








# -----------------------------------------------------------------------------
# 4. apply LDA to the cell line data
# -----------------------------------------------------------------------------

rm(list=ls())
graphics.off()



# 4.1 Import data
source(paste(MLTB_dir, "d_LDA.R", sep=.Platform$file.sep))
source(paste(MLTB_dir, "filter_SCLC.R", sep=.Platform$file.sep))



in_file_name <- "SCLC_study_output.csv"

dataIn <- read.csv(file=in_file_name,
                   header=T,
                   sep=",")

data_for_analysis <- filter_SCLC(dataIn)







# 4.2 apply LDA
data_for_analysis <- as.matrix(data_for_analysis)




# LDA
W <- d_LDA(x1=data_for_analysis[1:20,], x2=data_for_analysis[21:40,])

W <- W / sqrt(sum(W^2)) # normalize W





# project each cell line onto W
Y <- matrix(W, nrow=1) %*% t(data_for_analysis)

# plot the projection
plot(Y[1:20], rep(0, times=20),
     pch=16, cex=1,
     col="blue",
     xlim=c(min(Y), max(Y)),
     xlab="projection on W", ylab="",
     main="LDA: projection of cell line data onto W")

points(Y[21:40], rep(0, times=20),
       pch=16, cex=1,
       col="red")

legend("topright", legend=c("NSCLC", "SCLC"), pch=c(16, 16), col=c("blue", "red"))





# examine W and the associated variables
# plot W
plot(W, pch=16, cex=1)
points(c(0, length(W)), c(0, 0), type="l")
text(1:length(W), W, labels=1:length(W), pos=3)





# plot variables
variable_ind <- 1
offset <- 0.2

plot(data_for_analysis[1:20,variable_ind], rep((1-offset), times=20),
     pch=16, cex=1,
     col="blue",
     xlim=c(min(data_for_analysis), max(data_for_analysis)),
     ylim=c(0, 19),
     xlab="variable values", ylab="variable index")
points(data_for_analysis[21:40, variable_ind], rep((1+offset), times=20),
       pch=16, cex=1,
       col="red")
points(c(0, max(data_for_analysis)), c(1, 1), type="l")

for (i in 2:19) {
    variable_ind <- i
    
    points(data_for_analysis[1:20,variable_ind], rep((i-offset), times=20),
           pch=16, cex=1,
           col="blue")
    points(data_for_analysis[21:40, variable_ind], rep((i+offset), times=20),
           pch=16, cex=1,
           col="red")
    points(c(0, max(data_for_analysis)), c(i, i), type="l")
}






# 4.3 standardize the variables
data_standardized <- matrix(0, nrow=40, ncol=19)
for (i in 1:19) {
    data_standardized[,i] <- (data_for_analysis[,i] - mean(data_for_analysis[,i]))/sd(data_for_analysis[,i])
}


# do LDA
W_2 <- d_LDA(x1=data_standardized[1:20,], x2=data_standardized[21:40,])

W_2 <- W_2 / sqrt(sum(W_2^2)) # normalize W




# project each cell line onto W_2
Y_2 <- matrix(W_2, nrow=1) %*% t(data_standardized)

# plot the projection
plot(Y_2[1:20], rep(0, times=20),
     pch=16, cex=1,
     col="blue",
     xlim=c(min(Y_2), max(Y_2)),
     xlab="projection on W", ylab="",
     main="LDA: projection of cell line data onto W")

points(Y_2[21:40], rep(0, times=20),
       pch=16, cex=1,
       col="red")
legend("topright", legend=c("NSCLC", "SCLC"), pch=c(16, 16), col=c("blue", "red"))




# examine W and the variables
plot(W_2, pch=16, cex=1)
points(c(0, length(W_2)), c(0, 0), type="l")
text(1:length(W_2), W_2, labels=1:length(W_2), pos=3)



variable_ind <- 1
plot(data_standardized[1:20,variable_ind], rep((1-offset), times=20),
     pch=16, cex=1,
     col="blue",
     xlim=c(min(data_standardized), max(data_standardized)),
     ylim=c(0, 19),
     xlab="variable values", ylab="variable index")
points(data_standardized[21:40, variable_ind], rep((1+offset), times=20),
       pch=16, cex=1,
       col="red")
points(c(min(data_standardized), max(data_standardized)), c(1, 1), type="l")

for (i in 2:19) {
    variable_ind <- i
    
    points(data_standardized[1:20,variable_ind], rep((i-offset), times=20),
           pch=16, cex=1,
           col="blue")
    points(data_standardized[21:40, variable_ind], rep((i+offset), times=20),
           pch=16, cex=1,
           col="red")
    points(c(min(data_standardized), max(data_standardized)), c(i, i), type="l")
}






# 4.4 compare LDA with PCA
re.PCA <- prcomp(x=data_for_analysis, scale=F)




# plot PC1 vs PC2
plot(re.PCA$x[,1], re.PCA$x[,2],
     pch=16, cex=1,
     asp=1,
     xlab="PC1", ylab="PC2",
     main="PCA without standardization of variables")
points(re.PCA$x[21:40, 1], re.PCA$x[21:40, 2],
       pch=16, cex=1,
       col="red")
legend("topright", legend=c("NSCLC", "SCLC"), pch=c(16, 16), col=c("black", "red") )






# PCA of standardized variables
re.PCA <- prcomp(x=data_for_analysis, scale=T)



# plot PC1 vs PC2
plot(re.PCA$x[,1], re.PCA$x[,2],
     pch=16, cex=1,
     asp=1,
     xlab="PC1", ylab="PC2",
     main="PCA with standardization of variables")
points(re.PCA$x[21:40, 1], re.PCA$x[21:40, 2],
       pch=16, cex=1,
       col="red")
legend("topright", legend=c("NSCLC", "SCLC"), pch=c(16, 16), col=c("black", "red") )








# 4.5 prediction
# use the first cell line for testing
row_index_for_test <- 1
test_data <- data_for_analysis[row_index_for_test,]

W <- d_LDA(x1=data_for_analysis[2:20,], x2=data_for_analysis[21:40,])


W <- W / sqrt(sum(W^2)) # normalize W





# project each cell line onto W
Y <- matrix(W, nrow=1) %*% t(data_for_analysis)

# plot the projection
plot(Y[1:20], rep(0, times=20),
     pch=16, cex=1,
     col="blue",
     xlim=c(min(Y), max(Y)),
     xlab="projection on W", ylab="",
     main="LDA: projection of cell line data onto W")

points(Y[21:40], rep(0, times=20),
       pch=16, cex=1,
       col="red")





# project the first cell line onto W
projection_of_test_data <- matrix(W, nrow=1) %*% matrix(test_data, ncol=1)
points(projection_of_test_data, 0,
       pch=16, cex=1,
       col="green")

legend("topright", 
       legend=c("NSCLC", "SCLC", "testing data"), 
       pch=c(16, 16, 16), 
       col=c("blue", "red", "green"))











# 5. r packages for LDA

# 5.1 apply LDA function in MASS to the iris data
packageName <- "MASS"
install.packages(packageName)
library(packageName, character.only = T)






re.LDA.MASS <- lda(x=iris[,1:4], grouping=iris$Species, tol=1.0e-4, method="mle", CV=F)
plot(re.LDA.MASS)
re.LDA.MASS$means
re.LDA.MASS$scaling # the transformation matrix



# get the projections
Y <- t(re.LDA.MASS$scaling) %*% t(iris[, 1:4])
Y <- t(Y)


# plot the projections
II_setosa <- which(iris$Species=="setosa")
II_versicolor <- which(iris$Species=="versicolor")
II_virginica <- which(iris$Species=="virginica")

plot(Y[II_setosa,1], Y[II_setosa,2],
     pch=16, cex=1,
     xlim=c(min(Y[,1]), max(Y[, 1])),
     ylim=c(min(Y[,2]), max(Y[,2])),
     col="red",
     xlab="LD1", ylab="LD2",
     main="LDA: iris projection onto the first two linear discrimants")

points(Y[II_versicolor, 1], Y[II_versicolor, 2],
       pch=16, cex=1,
       col="green")

points(Y[II_virginica, 1], Y[II_virginica, 2],
       pch=16, cex=1,
       col="blue")

legend("topright", legend=c("setosa", "versicolor", "virginica"), 
       pch=c(16, 16, 16), 
       col=c("red", "green", "blue"))







# 5.2 apply LDA function in MASS to the cell line data
grouping_info <- factor(c(rep(0, times=20), rep(1, times=20)))
re.LDA.MASS <- lda(x=data_for_analysis, grouping=grouping_info, tol=1.0e-4, method="mle", CV=F)





# get the projections
Y <- t(re.LDA.MASS$scaling) %*% t(data_for_analysis)
Y <- t(Y)


# plot the projections
offset <- 0.1
plot(Y[1:20], rep(0-offset, times=20),
     pch=16, cex=1,
     xlim=c(min(Y), max(Y)), ylim=c(-1,1),
     col="blue",
     xlab="projection onto LD1", ylab="",
     main="projection of cell line data onto W")
points(Y[21:40], rep(0+offset, times=20),
       pch=16, cex=1,
       col="red")

legend("topright", legend=c("NSCLC", "SCLC"), 
       pch=c(16, 16), 
       col=c("blue", "red"))






# 5.3 apply LDA function in DiscriMiner to the iris data
packageName <- "DiscriMiner"
install.packages(packageName)
library(packageName, character.only = T)







re.LDA.DiscriMiner <- linDA(variables=iris[,1:4], 
                            group=iris$Species, 
                            validation="crossval")

re.LDA.DiscriMiner$functions
re.LDA.DiscriMiner$confusion
re.LDA.DiscriMiner$scores
re.LDA.DiscriMiner$classification
re.LDA.DiscriMiner$error_rate
