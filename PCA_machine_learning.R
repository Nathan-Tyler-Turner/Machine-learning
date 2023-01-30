#PCA 


#Tyler Turner







#problem number 2

My_PCA_Function <- function(DataInput){
  #read in the table
  DataIn <- read.csv("Homework_2_dataset_prob3.csv", header = TRUE, sep = ",", dec= ".")

   #obtain the 2 columns of the data
  DataIn <- DataIn[,1:2]
   
   #perform mean centering for the matrix
  mean_Centered <- scale(DataIn, scale=F)
   
   #calculate the covariance matrix of the mean centered data
  Covariance <- round(cov(mean_Centered),30)
   
   #find the eigen vectors of that matrix
  eigenVectors <- eigen(Covariance)$vectors
   
   #perform the projection of the matrix
  Projection <- cbind(eigenVectors[,1:2])
   
   #find the scores of that matrix by multiplying mean centered matrix with projection
  scoresMatrix <- mean_Centered %*% Projection
   
   #display results
  mydata <- list(mean_centered=mean_Centered, 
                 Covariance=Covariance, 
                 eigenvectors=eigenVectors, 
                 projection=Projection, 
                 scoresmatrix=scoresMatrix)
  
  View(scoresMatrix)
  return(mydata)
}


#call function, display will be opened in a new tab
tester <- My_PCA_Function(table)

tester



#problem number 3

DataIn <- read.csv("Homework_2_dataset_prob3.csv", header = TRUE, sep = ",")

#obtain the 2 columns of the data
DataIn <- DataIn[,1:2]

x <- DataIn[,1]

y <- DataIn[,2]

s <- cbind(x, y)
processed <- My_PCA_Function(s)

a <- processed[,1]
b <- processed[,2]

z <- cbind(a,b)

plot(x, y, asp=1, 
     pch=16, cex=1, col="blue",
     main="raw data and PC axis", 
     xlab="x", ylab="y",
     xlim=c(-1,1)) # plot raw data

#run pca analysis of homework data set prob3
plot(a, b, pch=16, cex=1, 
     main="Scores plot",
     xlab="PC number", ylab="variance",
     xlim = c(-25,25),
     ylim = c(-25,25))


#4

DataIn <- read.csv("Homework_2_dataset_prob4.csv", header = TRUE, sep = ",")
test <- t(DataIn)


My_PCA_Function <- function(DataInput){
  #read in the table
  DataIn <- read.csv(DataInput)
  
  DataIn<- t(DataIn)
  
  #obtain the 2 columns of the data
  
  #perform mean centering for the matrix
  mean_Centered <- scale(DataIn, center =T, scale=F)
  
  #calculate the covariance matrix of the mean centered data
  Covariance <- round(cov(mean_Centered),30)
  
  #find the eigen vectors of that matrix
  eigenVectors <- eigen(Covariance)$vectors
  
  #perform the projection of the matrix
  Projection <- cbind(eigenVectors[,1:2])
  
  #find the scores of that matrix by multiplying mean centered matrix with projection
  scoresMatrix <- mean_Centered %*% Projection
  
  #display results
  mydata <- list(mean_centered=mean_Centered, 
                 Covariance=Covariance, 
                 eigenvectors=eigenVectors, 
                 projection=Projection, 
                 scoresmatrix=scoresMatrix)
  
  View(scoresMatrix)
  return(mydata)
}


tester <- My_PCA_Function("C:/Users/Tyler/Documents/Homework_2_dataset_prob4.csv")

tester



