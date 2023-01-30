#Tyler Turner
#eigen vectors and eigen values


path_to_R_libs <- "/Users/Tyler/Documents/R_libs"
MLTB_dir <- "C:/Users/Tyler/Desktop/R codes/MLTB" 
data_dir <- "C:/Users/Tyler/Documents"

setwd(data_dir)

packageName <- "class"
if (!is.element(el=packageName, set=installed.packages(lib.loc=path_to_R_libs)[,1])) {
  install.packages(pkgs=packageName, 
                   lib=path_to_R_libs, 
                   repos="http://cran.us.r-project.org",
                   dependencies=T)
}
library(package=packageName,
        lib.loc=path_to_R_libs,
        character.only=T)

install.packages(pkgs=packageName)
library(packageName,
        character.only = T)


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

data_for_analysis <- W

source(paste(MLTB_dir, "d_loocv_knn.R", sep=.Platform$file.sep))

re.loocv.knn <- d_loocv_knn(x=data_for_analysis, y=factor( c(rep(1, times=20), rep(2, times=20)) ))
