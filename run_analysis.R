###---------------------------------------------###
#  Reading in the Data
###---------------------------------------------###

setwd("C:/Users/Laura/Google Drive/Data Science Specialization Coursera/Getting and Cleaning Data/Project")

#temp <- list.files(pattern = '*.txt')

#for (i in 1:length(temp)) assign(gsub('.txt','', temp[i]), read.csv(temp[i], header=FALSE))

X_test <- read.csv("X_test.txt", header=FALSE)
X_train <- read.csv("X_train.txt", header=FALSE)
y_test <- read.csv("y_test.txt", header=FALSE)
y_train <- read.csv("y_train.txt", header=FALSE)
subject_test <- read.csv("subject_test.txt", header=FALSE)
subject_train <- read.csv("subject_train.txt", header=FALSE)

setwd("C:/Users/Laura/Google Drive/Data Science Specialization Coursera/Getting and Cleaning Data/Project/Readme")

features <- read.csv("features.txt", sep="", header=FALSE)
features[,2] <- as.character(features[,2])
labels <- read.csv("activity_labels.txt", sep=" ", header=FALSE)

###---------------------------------------------###
#  Merging the test and train features data sets
###---------------------------------------------###


#10) X

X_all<-rbind(X_test,X_train)

X_all[,1] <- as.character(X_all[,1])

X_all<-strsplit(X_all[,1], split=as.vector(" "))



for (i in 1:length(X_all))
{
  X_tran <- as.data.frame(X_all[i],col.names = "featuresvalues")
  X_tran[,1] <- as.character(X_tran[,1])
  X_tran <- as.data.frame(X_tran[which(nchar(X_tran$featuresvalues)>0),])
  #X_tran[,1] <- as.numeric(X_tran[,1])
  X_tran <- t(X_tran)
  if (i==1) {X <- X_tran }
  else {X <- rbind(X, X_tran) }
}

#checking
nrow(X)
ncol(X)

X_test <-NULL
X_train <-NULL
X_tran <- NULL

row.names(X)<- 1:nrow(X)

###--------------------------------------------------------------------###
#  Extracting only the measurements needed
###--------------------------------------------------------------------###
names <- grep("mean|std", features[,2])
features[names,2]
names(X) <- features[,2]

X <- X[,names]
X <- as.data.frame(X)
X$id <- 1:nrow(X)
names(X) <- c(features[names,2],"id")

#remove meanFreq
names2 <- grepl("meanFreq", names(X))
X <- X[,!names2]


###---------------------------------------------------------------------###
#  Merging the test and train data sets for subjects and activity labels
###---------------------------------------------------------------------###


#11) Y

Y_all<-rbind(y_test,y_train)
Y_all$id<- 1:nrow(Y_all)


Y_labels <- merge(Y_all, labels, by.x="V1", by.y="V1", all.x=TRUE)
Y_labels <- Y_labels[order(Y_labels$id),]

y_test <- NULL
y_train <- NULL

#12) Subject
subject_all<-rbind(subject_test,subject_train)
subject_all$id <- 1:nrow(subject_all)

labels_subject <- merge(Y_labels,subject_all, by="id")
labels_subject <- labels_subject[,c(1,3,4)]
names(labels_subject) <- c("id", "activity_name", "subject")

subject_test <- NULL
subject_train <- NULL

###---------------------------------------------------------------------###
#  Adding the activity labels to the features data
###---------------------------------------------------------------------###

finaloutput <- merge(X, labels_subject, by="id")

###---------------------------------------------------------------------###
#  Converting to numeric and aggregating at subject-activity level
###---------------------------------------------------------------------###

for (i in 2:(ncol(finaloutput)-2))
{
  finaloutput[,i] <- as.numeric(levels(finaloutput[,i]))[finaloutput[,i]]
}

finaloutput$subject <- as.factor(finaloutput$subject)

for (i in 2:(ncol(finaloutput)-2))
{
  if(i==2)
  {aggregatedoutput <- aggregate(finaloutput[,i]~subject + activity_name, data=finaloutput, FUN=mean)}
  else 
  {aggregate <- aggregate(finaloutput[,i]~subject + activity_name, data=finaloutput, FUN=mean)
  aggregatedoutput <- merge(aggregatedoutput, aggregate, by=c("subject", "activity_name"))
  }
}

names(aggregatedoutput)[3:ncol(aggregatedoutput)] <- names(X)[1:ncol(X)-1]

write.csv(aggregatedoutput,"mean_per_activity_subject.csv")

