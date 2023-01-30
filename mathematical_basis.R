# mathematical_basis.R

# Author: Xiuxia Du And Tyler Turner






rm(list=ls())





# norm
v <- rnorm(10)
sqrt(sum(v^2))
norm(v, type="2")






# calculate the eigenvalues and eigenvectors of a matrix
A <- matrix(c(2,1,1,2), nrow=2, byrow=T)
s <- eigen(A)
s$values
s$vectors



A <- matrix(c(1, 0, 1, 3), nrow=2, byrow=T)
s <- eigen(A)
solve(s$vectors) %*% A %*% s$vectors







# Discrete expectation
N <- 1000
n <- 10
p <- 0.1

plot(0:n, dbinom(x=0:n, size=n, prob=p),
     type="h",
     col="blue")


x <- rbinom(n=N, size=n, prob=p)
plot(x, 1:length(x)/N * max(dbinom(x=0:n, size=n, prob=p)), 
     pch=16, cex=1,
     col="blue",
     ylim=c(-max(dbinom(x=0:n, size=n, prob=p)), max(dbinom(x=0:n, size=n, prob=p))),
     xlab="k", ylab="")
points(0:n, -dbinom(x=0:n, size=n, prob=p),
     type="h",
     lwd=3,
     col="red")




mean(x)
var(x)






# Continuous expectation
mu <- 5
sigma <- 2

x <- seq(from=mu-5*sigma, to=mu+5*sigma, by=0.01)
plot(x, dnorm(x, mean=mu, sd=sigma),
     type="l", col="red", lwd=2,
     xlab="x", ylab="f(x)")
points(c(mu, mu), c(0, dnorm(x=mu, mean=mu, sd=sigma)),
       type="l", col="blue", lwd=1)



mean(x)
var(x)






# covariance
N <- 1000
x <- runif(n=N, min=-10, max=10)



y <- 2*x + rnorm(n=N)

plot(x, y,
     pch=16, cex=1,
     col="blue")

cov(x, y)
cor(x,y)


z <- x^2
plot(x, z,
     pch=16, cex=1,
     col="blue")

cov(x, z)
cor(x, z)





# covariance matrix
B <- cbind(x, z)
cov(B)



