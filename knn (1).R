
# Author: Xiuxia Du And Tyler Turner






rm(list=ls())
graphics.off()



path_to_R_libs <- "/Users/Tyler/Documents/R_libs"
MLTB_dir <- "/Users/Tyler/Desktop/R codes/MLTB"
data_dir <- "/Users/Tyler/Documents/SCLC_study_output.csv"





par_default <- par()
par(pin=c(3,3))
par(mar=c(1,1,1,1))







# =======================================
# k nearest neighbor illustrated using the iris data
# =======================================

# This package contains various functions for classification, 
# including k-nearest neighbors, learning vector quantization, and self-organizing maps.
packageName <- "class"
if (!is.element(el=packageName, set=installed.packages(lib.loc=path_to_R_libs)[,1])) {
  install.packages(pkgs=packageName, 
                   lib=path_to_R_libs, 
                   repos="http://cran.us.r-project.org",
                   dependencies=TRUE)
}
library(package=packageName,
        lib.loc=path_to_R_libs,
        character.only=TRUE)



# OR use this to install packages
install.packages(pkgs=packageName)
library(packge=packageName)





# 1. apply knn to the iris dataset
data()
# look for the iris data set

# 1.1 visualize the data
str(iris)
table(iris$Species)

unique_flower_types <- as.factor(c("setosa", "versicolor", "virginica"))





# sepal data
plot(iris$Sepal.Length, iris$Sepal.Width,
     pch=16, cex=1,
     main="sepal data",
     xlab="sepal length", ylab="sepal width")

color_vector <- c("blue", "red", "green")
for (i in 1:length(color_vector)) {
    II <- which(iris$Species==unique_flower_types[i])
    points(iris$Sepal.Length[II], iris$Sepal.Width[II],
           pch=16, cex=1,
           col=color_vector[i])
}
legend("topright", legend=unique_flower_types, 
       pch=16, cex=1,
       col=color_vector )
# There is a high correlation between sepal length and sepal width for setosa flowers, while
# the correlation is less high for the virginica and versicolor flowers.





# petal data
plot(iris$Petal.Length, iris$Petal.Width,
     pch=16, cex=1,
     main="petal data",
     xlab="petal length", ylab="pepal width")

color_vector <- c("blue", "red", "green")
for (i in 1:length(color_vector)) {
    II <- which(iris$Species==unique_flower_types[i])
    points(iris$Petal.Length[II], iris$Petal.Width[II],
           pch=16, cex=1,
           col=color_vector[i])
}
legend("topright", legend=unique_flower_types, 
       pch=16, cex=1,
       col=color_vector )
# There is a correlation between petal length and petal width for all species of the flowers.







# 1.2 normalization
summary(iris)

my_normalization <- function(x) {
    y <- (x - min(x)) / (max(x) - min(x))
    return(y)
}

normalized_iris <- as.data.frame(lapply(iris[,1:4], my_normalization) )

summary(normalized_iris)







# 1.3 get familiar with the sample() function

# ramdom permuation
d <- 1:10
sample(x=d)


repeat_times <- 5
for (i in 1:repeat_times) {
    re <- sample(x=d)
    print(re)
}



# without replacement
k <- 6
sample(x=d, size=k, replace=F) 


for (i in 1:repeat_times) {
    re <- sample(x=d, size=k, replace=F) 
    print(re)
}




# with replacement
sample(x=d, size=k, replace=T)

for (i in 1:repeat_times) {
    re <- sample(x=d, size=k, replace=T) 
    print(re)
}



# bootstrap
sample(x=d, replace=T)

for (i in 1:repeat_times) {
    re <- sample(x=d, replace=T) 
    print(re)
}




# 100 bernoulli trials
n <- 100
num_vector <- c(0, 1)
prob_vector <- c(2/3, 1/3)
re <- sample(x=num_vector, size=n, replace=T, prob=prob_vector) 
re
prop_0 <- length(which(re==0)) / length(re)
prop_0




y <- vector(mode="list", length=repeat_times)
for (i in 1:repeat_times) {
    re <- sample(x=num_vector, size=n, replace=T, prob=prob_vector) 
    prop_0 <- length(which(re==0)) / length(re)
    
    y[[i]]$vector <- re
    y[[i]]$prop_0 <- prop_0
}




# sample()'s surprise

sample(x=d[d>8])
sample(x=d[d>9])
sample(x=d[d>10])


# use sample.int() to avoid these surprises
sample.int(n=length(d[d>8]))

resample <- function(x) {
    x[sample.int(length(x))]
}

resample(d[d>8])
resample(d[d>9])
resample(d[d>10])









# 1.4 select training and test data using random sampling

set.seed(1234)

II <- sample(x=c(1,2), size=nrow(iris), replace=T, prob=c(0.67, 0.33) )

iris.training <- iris[II==1, 1:4]
iris.test <- iris[II==2, 1:4]

iris.training.labels <- iris[II==1, 5]
iris.test.labels <- iris[II==2, 5]

re.knn <- knn(train=iris.training, 
              test=iris.test, 
              cl=iris.training.labels,
              k=3)

re.knn




# 1.5 evaluation of the model
temp <- data.frame(observed_species = iris.test.labels, predicted_species=re.knn)
temp
# one incorrect classification

compare_results <- mapply(identical, temp[,1], temp[,2])
compare_results

error_rate <- length(which(compare_results==F)) / length(temp[,1])





# =======================================
# evaluation of classifier accuracy
# =======================================

# 1. holdout method
# 1.1 holdout and repeated holdout
repeat_times <- 100
re.knn <- vector(mode="list", length=repeat_times)

for (i in 1:repeat_times) {
    II <- sample(x=c(1,2), size=nrow(iris), replace=TRUE, prob=c(0.67, 0.33) )
    
    iris.training <- iris[II==1, 1:4]
    iris.test <- iris[II==2, 1:4]
    
    iris.training.labels <- iris[II==1, 5]
    iris.test.labels <- iris[II==2, 5]
    
    re.knn[[i]]$knn <- knn(train=iris.training, 
                           test=iris.test, 
                           cl=iris.training.labels,
                           k=3)   
    
    compare_results <- mapply(identical, re.knn[[i]]$knn, iris.test.labels)
    error_rate <- length(which(compare_results==FALSE)) / length(iris.test.labels)
    re.knn[[i]]$number_of_false_classification <- length(which(compare_results==FALSE))
    re.knn[[i]]$error_rate <- error_rate
}



error_rate_vector <- vector(mode="numeric", length=repeat_times)

for (i in 1:repeat_times) {
    error_rate_vector[i] <- re.knn[[i]]$error_rate
}
overall_error_rate <- mean(error_rate_vector)
overall_error_rate





# 1.2 stratification with the holdout method
sp <- split(x=iris, f=iris$Species)
# split the iris data into three groups according to the species




prob_vector <- c(2/3, 1/3)

holdout_sampling <- function(x) {
    II <- sample(x=c(1,2), size=nrow(x), replace=TRUE, prob=prob_vector)
    return(II)
}
sampling_id <- lapply(sp, holdout_sampling) # random sampling of each group in sp





# obtain the training set and test set by combining the three groups
iris.training <- data.frame()
iris.test <- data.frame()
iris.training.labels <- factor(x=character(), levels=unique_flower_types)
iris.test.labels <- factor(x=character(), levels=unique_flower_types)


for (i in 1:length(unique_flower_types)) {
    temp_data <- sp[[ unique_flower_types[i] ]]
    temp_id <- sampling_id[[ unique_flower_types[i] ]]
    
    II_training <- which(temp_id == 1)
    iris.training <- rbind(iris.training, temp_data[II_training,1:4])
    iris.training.labels <- factor( c(levels(iris.training.labels)[iris.training.labels], 
                                      levels(temp_data[II_training,5])[temp_data[II_training,5]]) )
    
    II_test <- which(temp_id == 2)
    iris.test <- rbind(iris.test, temp_data[II_test,1:4])
    iris.test.labels <- factor( c(levels(iris.test.labels)[iris.test.labels],
                                  levels(temp_data[II_test,5])[temp_data[II_test,5]]) )
}





# knn
re.knn <- knn(train=iris.training,
              test=iris.test,
              cl=iris.training.labels,
              k=3)



# error rate
temp <- data.frame(observed_species = iris.test.labels, predicted_species=re.knn)
temp

compare_results <- mapply(identical, temp[,1], temp[,2])

error_rate <- length(which(compare_results==FALSE)) / length(temp[,1])
error_rate





# 1.3 repeated holdout with stratification
source(paste(MLTB_dir, "my_stratification.R", sep=.Platform$file.sep))

repeat_times <- 100
prob_vector <- c(2/3, 1/3)
re.knn <- vector(mode="list", length=repeat_times)
for (i in 1:repeat_times) {
    re.stratification <- my_stratification(x=iris, prob_vec=prob_vector)
    
    
    re.knn[[i]]$knn <- knn(train=re.stratification$training,
                           test=re.stratification$test,
                           cl=re.stratification$training.labels,
                           k=3)
    
    
    compare_results <- mapply(identical, re.knn[[i]]$knn, re.stratification$test.labels)
    error_rate <- length(which(compare_results==FALSE)) / length(re.stratification$test.labels)
    re.knn[[i]]$number_of_false_classification <- length(which(compare_results==FALSE))
    re.knn[[i]]$error_rate <- error_rate
}



error_rate_vector <- vector(mode="numeric", length=repeat_times)

for (i in 1:repeat_times) {
  error_rate_vector[i] <- re.knn[[i]]$error_rate
}
overall_error_rate <- mean(error_rate_vector)
overall_error_rate




# 2. cross-validation
# 2.1 leave-one-out CV
re.knn <- vector(mode="list", length=nrow(iris))
for (i in 1:nrow(iris)) {
  
    if (i==1) {
        iris.test <- iris[1, 1:4]
        iris.test.labels <- iris[1, 5]
        
        iris.training <- iris[2:nrow(iris), 1:4]
        iris.training.labels <- iris[2:nrow(iris), 5]
        
    } else if (i==nrow(iris)) {
        iris.test <- iris[i, 1:4]
        iris.test.labels <- iris[i, 5]
        
        iris.training <- iris[1:(nrow(iris)-1), 1:4]
        iris.training.labels <- iris[1:(nrow(iris)-1), 5]
        
    } else {
        iris.test <- iris[i, 1:4]
        iris.test.labels <- iris[i, 5]
        
        iris.training <- rbind(iris[1:(i-1), 1:4],
                               iris[(i+1):nrow(iris), 1:4])
        
        iris.training.labels <- factor( c(levels(iris[1:(i-1), 5])[iris[1:(i-1), 5]] ,
                                          levels(iris[(i+1):nrow(iris), 5])[iris[(i+1):nrow(iris), 5]]) )
    }
    
    
    re.knn[[i]]$knn <- knn(train=iris.training,
                           test=iris.test,
                           cl=iris.training.labels,
                           k=3)
    
    compare_results <- identical(iris.test.labels, re.knn[[i]]$knn )
    
    
    error_rate <- length(which(compare_results==FALSE)) / length(compare_results)
    re.knn[[i]]$number_of_false_classification <- length(which(compare_results==FALSE))
    re.knn[[i]]$error_rate <- error_rate
  
}

error_rate_vector <- vector(mode="numeric", length=nrow(iris))

for (i in 1:nrow(iris)) {
  error_rate_vector[i] <- re.knn[[i]]$error_rate
}
overall_error_rate <- mean(error_rate_vector)








# 2.2 k-fold cross-validation
source(paste(MLTB_dir, "d_stratified_crossvalidation.R", sep=.Platform$file.sep))


number_of_fold <- 5
re.stratified_CV <- d_stratified_crossvalidation(x=iris, k=number_of_fold)

re.knn <- vector(mode="list", length=number_of_fold)
for (fold in 1:length(re.stratified_CV)) {
    re.knn[[fold]]$knn <- knn(train=re.stratified_CV[[fold]]$training,
                              test=re.stratified_CV[[fold]]$test,
                              cl=re.stratified_CV[[fold]]$training.labels,
                              k=3)
    
    compare_results <- mapply(identical, re.stratified_CV[[fold]]$test.labels, re.knn[[fold]]$knn)
    
    error_rate <- length(which(compare_results==F)) / length(compare_results)
    re.knn[[fold]]$number_of_false_classification <- length(which(compare_results==F))
    re.knn[[fold]]$error_rate <- error_rate
}

error_rate_vector <- vector(mode="numeric", length=number_of_fold)

for (i in 1:length(re.knn)) {
  error_rate_vector[i] <- re.knn[[i]]$error_rate
}
overall_error_rate <- mean(error_rate_vector)







# 3. bootstrap
repeat_times <- 1000
re.knn <- vector(mode="list", length=repeat_times)
for (i in 1:repeat_times) {
    II <- sample(x=1:nrow(iris), replace=T)
    
    II_unique_samples_for_training <- sort(unique(II))
    II_samples_for_testing <- setdiff(1:nrow(iris), II_unique_samples_for_training)
    
    iris.training <- iris[II, 1:4]
    iris.training.labels <- iris[II, 5]
    
    iris.test <- iris[II_samples_for_testing, 1:4]
    iris.test.labels <- iris[II_samples_for_testing, 5]
    
    re.knn[[i]]$knn <- knn(train=iris.training,
                           test=iris.test,
                           cl=iris.training.labels,
                           k=3)
    
    compare_results <- mapply( identical, iris.test.labels, re.knn[[i]]$knn )
    
    
    error_rate <- length(which(compare_results==F)) / length(compare_results)
    re.knn[[i]]$number_of_false_classification <- length(which(compare_results==F))
    re.knn[[i]]$error_rate <- error_rate
}


error_rate_vector <- vector(mode="numeric", length=nrow(iris))

for (i in 1:nrow(iris)) {
  error_rate_vector[i] <- re.knn[[i]]$error_rate
}
overall_error_rate <- mean(error_rate_vector)







# 4. contingency table
# This package contains various R programming tools for model fitting.
packageName <- "gmodels"
if (!is.element(el=packageName, set=installed.packages(lib.loc=path_to_R_libs)[,1])) {
  install.packages(pkgs=packageName, 
                   lib=path_to_R_libs, 
                   repos="http://cran.us.r-project.org",
                   dependencies=T)
}
library(package=packageName,
        lib.loc=path_to_R_libs,
        character.only=T)







set.seed(1234)

II <- sample(x=c(1,2), size=nrow(iris), replace=T, prob=c(0.67, 0.33) )

iris.training <- iris[II==1, 1:4]
iris.test <- iris[II==2, 1:4]

iris.training.labels <- iris[II==1, 5]
iris.test.labels <- iris[II==2, 5]

re.knn <- knn(train=iris.training, 
              test=iris.test, 
              cl=iris.training.labels,
              k=3)




re.cross_tabulation <- CrossTable(x=iris.test.labels, y=re.knn, prop.chisq = F)
re.cross_tabulation$t
re.cross_tabulation$prop.row
re.cross_tabulation$prop.col
re.cross_tabulation$prop.tbl











# =======================================
# k nearest neighbor applied to the cell line data
# =======================================


# 1 Import data
in_file_name <- "SCLC_study_output.csv"
in_file_full_name <- paste(data_dir, in_file_name, sep=.Platform$file.sep)

dataIn <- read.csv(file=in_file_full_name,
                   header=T,
                   sep=",",
                   check.names = F)



source(paste(MLTB_dir, "filter_SCLC.R", sep=.Platform$file.sep))
data_for_analysis <- filter_SCLC(dataIn)




# 3 classification
# 3.1 leave-one-out
source(paste(MLTB_dir, "d_loocv_knn.R", sep=.Platform$file.sep))

re.loocv_knn <- d_loocv_knn(x=data_for_analysis, y=factor( c(rep(1, times=20), rep(2, times=20)) ))





# find which cell line is wrongly classified
II <- which(re.loocv.knn$error_rate_vector==1)
sample_names <- rownames(data_for_analysis)
sample_names[II] # NSCLC_PC9_1 was wrongly classified





# 3.2 k-fold cross-validation
number_of_fold <- 5
re.crossvalidation <- d_stratified_crossvalidation(d, k=number_of_fold)

re.knn <- vector(mode="list", length=number_of_fold)
for (i in 1:length(re.crossvalidation)) {
    re.knn[[i]]$knn <- knn(train=re.crossvalidation[[i]]$training,
                           test=re.crossvalidation[[i]]$test,
                           cl=re.crossvalidation[[i]]$training.labels,
                           k=3)
    
    compare_results <- mapply( identical, re.crossvalidation[[i]]$test.labels, re.knn[[i]]$knn )
    
    
    error_rate <- length(which(compare_results==F)) / length(compare_results)
    re.knn[[i]]$number_of_false_classification <- length(which(compare_results==F))
    re.knn[[i]]$error_rate <- error_rate
}


error_rate_vector <- vector(mode="numeric", length=number_of_fold)

for (i in 1:number_of_fold) {
    error_rate_vector[i] <- re.knn[[i]]$error_rate
}
overall_error_rate <- mean(error_rate_vector)
