###############################################
#step 1 get data
###############################################
zipname <-"UCI HAR Dataset.zip"
zipUrl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
filename <-"UCI HAR Dataset"
# download zip file containing data if not already downloaded
if(!file.exists(zipname))
{
  download.file(zipUrl,zipname,method = "curl",mode ="wb")
}

#unzip if not already 

if(!file.exists(filename))
{
  unzip(zipname)
}


###################################################
#step 2 read data 
###################################################

# don't convert text labels to factors using as.is = true
features<-read.table(file.path(filename,"features.txt"),as.is = TRUE)
subject <-read.table(file.path(filename,"train","subject_train.txt"))
value <- read.table(file.path(filename,"train","x_train.txt"))
activity <- read.table (file.path(filename,"train","y_train.txt"))
activity_labels <- read.table (file.path(filename,"activity_labels.txt"))

value <- cbind(subject,activity,value)
colnames(value)<-c("subject","activity",features[,2])
value$activity<-factor(value$activity,levels = activity_labels[,1] ,labels = activity_labels[,2])
rm(features,subject,activity,activity_labels)

sub1<-subset(value,subject==1)
par(mfrow=c(2,2))
plot(sub1[,3],col=sub1$activity,ylab=names(sub1[3]))
plot(sub1[,4],col=sub1$activity,ylab=names(sub1[4]))
plot(sub1[,5],col=sub1$activity,ylab=names(sub1[5]))
plot(sub1[,6],col=sub1$activity,ylab=names(sub1[6]))

par(mfrow=c(1,1))
source("myplclust.r")
distancematrix<-dist(sub1[,3:5])
hclustering<-hclust(distancematrix)
myplclust(hclustering,lab.col=unclass(sub1$activity))


svd1<-svd(scale(sub1[,-c(1,2)]))
par(mfrow=c(1,2))
plot(svd1$u[,1],col=sub1$activity,pch=19)
plot(svd1$u[,2],col=sub1$activity,pch=19)
legend("bottomright",col = unique(sub1$activity), legend = unique(sub1$activity),pch=19)

plot(svd1$v[,1],col=sub1$activity,pch=19)
plot(svd1$v[,2],col=sub1$activity,pch=19)
legend("bottomright",col = unique(sub1$activity), legend = unique(sub1$activity),pch=19)

maxcontrib1<-which.max(svd1$v[,1])
maxcontrib2<-which.max(svd1$v[,2])
maxcontrib3<-which.max(svd1$v[,3])
maxcontrib4<-which.max(svd1$v[,4])

distanceMatrix <- dist(sub1)
hclustering <- hclust(distanceMatrix)
myplclust(hclustering, lab.col = unclass(sub1$activity))
legend("topright",col = unique(sub1$activity), legend = unique(sub1$activity),pch=19)

kClust <- kmeans(sub1[, c(-1,-2)], centers = 6)
table(kClust$cluster, sub1$activity)

  