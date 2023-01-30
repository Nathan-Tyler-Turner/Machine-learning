#Tyler Turner

#Kmeans clustering

data_in <- matrix(c(1, 1, 2, 1, 4, 3, 5, 4), ncol=2, byrow=T)

num_clust <- 2

max_iter <- 3

colMeans(data_in)

centroid_1 <- (data_in[1,])

centroid_2 <- (data_in[2,])

iter <- 0

my_kmeans_function <- function(x,y,z){
  
  
  group_1 <- cbind(data_in[1,1], data_in[1,2], data_in[2,1], data_in[2,2])
  
  group_2 <- cbind(data_in[3,1], data_in[3,2], data_in[4,1], data_in[4,2])
  
  
  while(iter < max_iter){
  
  dist <- NULL
  
  for(i in 1:nrow(group_1)) dist[i] <- euc.dist(group_1[i,],centroid_1[i,])
  
  if(dist > 1){
    group_2 <- cbind(group_1, i)
  }
  
  dist <- NULL
  
  
  for(i in 1:nrow(group_1)) dist[i] <- euc.dist(group_1[i,],centroid_2[i,])
  
  if(dist > 1){
    group_1 <- cbind(group_1, i)
  }
  
  dist <- NULL
  
  for(i in 1:nrow(group_2)) dist[i] <- euc.dist(group_2[i,],centroid_1[i,])
  
  if(dist > 1){
    group_1 <- cbind(group_1, i)
  }
  
  dist <- NULL
  
  
  for(i in 1:nrow(group_2)) dist[i] <- euc.dist(group_2[i,],centroid_2[i,])
  
  if(dist > 1){
    group_2 <- cbind(group_2, i)
  }
  
  centroid_1 <- colMeans(group_1)
  
  centroid_2 <- colMeans(group_2)

  iter++
  
  
  
  mydata <- list(data_in,num_clust,max_iter,centroid_1, centroid_2)
  
  return(mydata)
  }
}

my_kmeans_function(data_in, num_clust, max_iter)
