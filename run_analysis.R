library(plyr)
setwd("D:/")

features_labels<- read.table("/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/features.txt")
activity_labels<- read.table("/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/activity_labels.txt")
subject_train<-read.table("/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt")
train_labels<- read.table("/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt")
train_set <- read.table("/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt")
body_acc_x_train <- read.table("/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/Inertial Signals/body_acc_x_train.txt")
body_acc_y_train <- read.table("/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/Inertial Signals/body_acc_y_train.txt")
body_acc_z_train <- read.table("/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/Inertial Signals/body_acc_z_train.txt")
body_gyro_x_train <- read.table("/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/Inertial Signals/body_gyro_x_train.txt")
body_gyro_y_train <- read.table("/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/Inertial Signals/body_gyro_y_train.txt")
body_gyro_z_train <- read.table("/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/Inertial Signals/body_gyro_z_train.txt")
total_acc_x_train <- read.table("/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/Inertial Signals/total_acc_x_train.txt")
total_acc_y_train <- read.table("/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/Inertial Signals/total_acc_y_train.txt")
total_acc_z_train <- read.table("/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/Inertial Signals/total_acc_z_train.txt")
subject_test<-read.table("/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt")
test_labels<- read.table("/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/y_test.txt")
test_set <- read.table("/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt")
body_acc_x_test <- read.table("/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/Inertial Signals/body_acc_x_test.txt")
body_acc_y_test <- read.table("/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/Inertial Signals/body_acc_y_test.txt")
body_acc_z_test <- read.table("/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/Inertial Signals/body_acc_z_test.txt")
body_gyro_x_test <- read.table("/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/Inertial Signals/body_gyro_x_test.txt")
body_gyro_y_test <- read.table("/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/Inertial Signals/body_gyro_y_test.txt")
body_gyro_z_test <- read.table("/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/Inertial Signals/body_gyro_z_test.txt")
total_acc_x_test <- read.table("/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/Inertial Signals/total_acc_x_test.txt")
total_acc_y_test <- read.table("/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/Inertial Signals/total_acc_y_test.txt")
total_acc_z_test <- read.table("/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/Inertial Signals/total_acc_z_test.txt")

names(train_set) <- features_labels[,2]
names(test_set) <- features_labels[,2]

temp1 <- numeric(length = 0)
temp2 <- numeric(length = 0)
temp3 <- numeric(length = 0)
for (i in 1:length(features_labels[,2])){
  temp1 <- c(temp1,length(grep("mean",features_labels[,2][i])))
  temp2 <- c(temp2,length(grep("std",features_labels[,2][i])))
  temp3 <- c(temp3,length(grep("meanFreq",features_labels[,2][i])))
}
temp1 <- as.logical(temp1)
temp2 <- as.logical(temp2)
temp3 <- as.logical(temp3)
ntrain_set <- train_set[,(temp1 | temp2) & (!temp3)]
ntest_set <- test_set[,(temp1 | temp2) & (!temp3)]

ntrain_labels <- as.factor(train_labels[,1])
levels(ntrain_labels) <-activity_labels[,2]
ntest_labels <- as.factor(test_labels[,1])
levels(ntest_labels) <-activity_labels[,2]
ntrain_labels <- as.data.frame(ntrain_labels)
ntest_labels <- as.data.frame(ntest_labels)

names(ntrain_labels) <- "activity"
names(ntest_labels) <- "activity"



labels1 <- character(length = 0)
for (i in 1:length(body_acc_x_train[1,])){
  labels1 <- c(labels1,paste("body_acc_x",as.character(i),sep=""))
}
names(body_acc_x_train)<-labels1

labels2 <- character(length = 0)
for (i in 1:length(body_acc_y_train[1,])){
  labels2 <- c(labels2,paste("body_acc_y",as.character(i),sep=""))
}
names(body_acc_y_train)<-labels2

labels3 <- character(length = 0)
for (i in 1:length(body_acc_z_train[1,])){
  labels3 <- c(labels3,paste("body_acc_z",as.character(i),sep=""))
}
names(body_acc_z_train)<-labels3

labels4 <- character(length = 0)
for (i in 1:length(body_gyro_x_train[1,])){
  labels4 <- c(labels4,paste("body_gyro_x",as.character(i),sep=""))
}
names(body_gyro_x_train)<-labels4

labels5 <- character(length = 0)
for (i in 1:length(body_gyro_y_train[1,])){
  labels5 <- c(labels5,paste("body_gyro_y",as.character(i),sep=""))
}
names(body_gyro_y_train)<-labels5

labels6 <- character(length = 0)
for (i in 1:length(body_gyro_z_train[1,])){
  labels6 <- c(labels6,paste("body_gyro_z",as.character(i),sep=""))
}
names(body_gyro_z_train)<-labels6

labels7 <- character(length = 0)
for (i in 1:length(total_acc_x_train[1,])){
  labels7 <- c(labels7,paste("total_acc_x",as.character(i),sep=""))
}
names(total_acc_x_train)<-labels7

labels8 <- character(length = 0)
for (i in 1:length(total_acc_y_train[1,])){
  labels8 <- c(labels8,paste("total_acc_y",as.character(i),sep=""))
}
names(total_acc_y_train)<-labels8

labels9 <- character(length = 0)
for (i in 1:length(total_acc_z_train[1,])){
  labels9 <- c(labels9,paste("total_acc_z",as.character(i),sep=""))
}
names(total_acc_z_train)<-labels9

labels10 <- character(length = 0)
for (i in 1:length(body_acc_x_test[1,])){
  labels10 <- c(labels10,paste("body_acc_x",as.character(i),sep=""))
}
names(body_acc_x_test)<-labels10

labels11 <- character(length = 0)
for (i in 1:length(body_acc_y_test[1,])){
  labels11 <- c(labels11,paste("body_acc_y",as.character(i),sep=""))
}
names(body_acc_y_test)<-labels11

labels12 <- character(length = 0)
for (i in 1:length(body_acc_z_test[1,])){
  labels12 <- c(labels12,paste("body_acc_z",as.character(i),sep=""))
}
names(body_acc_z_test)<-labels12

labels13 <- character(length = 0)
for (i in 1:length(body_gyro_x_test[1,])){
  labels13 <- c(labels13,paste("body_gyro_x",as.character(i),sep=""))
}
names(body_gyro_x_test)<-labels13

labels14 <- character(length = 0)
for (i in 1:length(body_gyro_y_test[1,])){
  labels14 <- c(labels14,paste("body_gyro_y",as.character(i),sep=""))
}
names(body_gyro_y_test)<-labels14

labels15 <- character(length = 0)
for (i in 1:length(body_gyro_z_test[1,])){
  labels15 <- c(labels15,paste("body_gyro_z",as.character(i),sep=""))
}
names(body_gyro_z_test)<-labels15

labels16 <- character(length = 0)
for (i in 1:length(total_acc_x_test[1,])){
  labels16 <- c(labels16,paste("total_acc_x",as.character(i),sep=""))
}
names(total_acc_x_test)<-labels16

labels17 <- character(length = 0)
for (i in 1:length(total_acc_y_test[1,])){
  labels17 <- c(labels17,paste("total_acc_y",as.character(i),sep=""))
}
names(total_acc_y_test)<-labels17

labels18 <- character(length = 0)
for (i in 1:length(total_acc_z_test[1,])){
  labels18 <- c(labels18,paste("total_acc_z",as.character(i),sep=""))
}
names(total_acc_z_test)<-labels18

names(subject_train) <- "subject"
names(subject_test) <- "subject"

group_train<-as.data.frame(rep("train",length(subject_train[,1])))
group_test<-as.data.frame(rep("test",length(subject_test[,1])))

names(group_train) <- "group"
names(group_test) <- "group"

IS_train <- cbind(body_acc_x_train,body_acc_y_train,body_acc_z_train,body_gyro_x_train,body_gyro_y_train,body_gyro_z_train,total_acc_x_train,total_acc_y_train,total_acc_z_train)
IS_test <- cbind(body_acc_x_test,body_acc_y_test,body_acc_z_test,body_gyro_x_test,body_gyro_y_test,body_gyro_z_test,total_acc_x_test,total_acc_y_test,total_acc_z_test)

data_train <- cbind(subject_train,ntrain_labels,group_train,ntrain_set,IS_train)
data_test <- cbind(subject_test,ntest_labels,group_test,ntest_set,IS_test)

newdata <- rbind(data_train,data_test)

colm <- function(data){
  data<-data[,4:length(data[1,])]
  colMeans(data)
}
  
tidydata <- ddply(newdata,.(subject,activity),colm)

write.table(tidydata,file="/GIT/Getting-and-Cleaning-Data-Peer-Assessments/tidy data.txt", row.names = FALSE)
