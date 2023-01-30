# Author:Xiuxia Du And Tyler Turner








# =======================================
# kmeans on toy data
# =======================================
rm(list=ls())
graphics.off()


data_in <- matrix(c(1, 1, 2, 1, 4, 3, 5, 4), ncol=2, byrow=T)
re <- kmeans(data_in, centers=2, algorithm="Lloyd")





# =======================================
# kmeans on real biological data
# =======================================
rm(list=ls())
graphics.off()



path_to_R_libs <- "/Users/xdu4/Documents/Duxiuxia/R_libs"




par_default <- par()
par(pin=c(3,3))
par(mar=c(1,1,1,1))





# 1. Import data
setwd("/Users/Tyler/Documents")
in_file_name <- "SCLC_study_output.csv"
data_in <- read.csv(file=in_file_name,
                   header=T,
                   check.names = F)



# rows are peaks, columns are values associated with each sample
all_col_names <- colnames(data_in)
# remove dots in column names with a white space
all_col_names <- gsub(pattern="\\.", replacement=" ", x=all_col_names, perl=T)
colnames(data_in) <- all_col_names
variable_names <- data_in$`row ID`



# extract the peak area columns
tf <- grepl(pattern="NSCLC_", x=all_col_names) & grepl(pattern="area", x=all_col_names)
II <- which(tf==TRUE)

tf <- grepl(pattern="^SCLC_", x=all_col_names) & grepl(pattern="area", x=all_col_names)
JJ <- which(tf==TRUE)



# get the peak area data
data <- cbind(data_in[, II], data_in[, JJ])
sample_names <- colnames(data)



crop_name <- function(s) {
  ind <- regexpr(pattern="_POS", text=s)
  return(substr(x=s, start=1, stop=ind-1))
}

sample_names_cropped <- sapply(sample_names, crop_name)
colnames(data) <- sample_names_cropped





# 2. Filter variables
tf <- grepl(pattern="row number of detected peaks", x=all_col_names)
II <- which(tf==TRUE)


JJ <- which(data_in[, II] >= 40) # select variables that are detected in all of the samples
data_for_analysis <- data[JJ, ]
data_for_analysis <- as.data.frame(t(data_for_analysis))
colnames(data_for_analysis) <- variable_names[JJ]






# 3. kmeans clustering
re.kmeans <- kmeans(x=data_for_analysis, centers=data_for_analysis[1:2,], algorithm="Lloyd", iter.max=100)
re.kmeans$size
re.kmeans$cluster




# 4. kmeans after PCA
re.PCA <- prcomp(x=data_for_analysis, scale=TRUE)
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
