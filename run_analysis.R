## main function is run_function, it simply get the script line running.
## the function will automatically download the samsung raw files, clean the data set
## and return the tidy dataset as well as write it into data.txt in your R working directory
## source the run_analysis.R and click Run the function run_function() in R console.

run_function<-function(){
## download and unzip data
  message("Checking if the compressed Samsung Dataset already exists...")
if (!file.exists("./getdata-projectfiles-UCI HAR Dataset.zip")){
  message("File doesn't exist, downloading...")
  download.file("http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",
                destfile="./getdata-projectfiles-UCI HAR Dataset.zip")
}

message("Download finished. Start unzipping...")
unzip("./getdata-projectfiles-UCI HAR Dataset.zip",overwrite = TRUE)


## Read raw file
# read activity and subject
message("Loading raw file. It might takes a while, depending on your computer configuration.")
activity_labels<-read.table("./getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/activity_labels.txt")
y_test<-read.table("./getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/y_test.txt",
                   col.names = "activity")
subject_test<-read.table("./getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt",
                         col.names = "subject")
y_train<-read.table("./getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt",
                    col.names = "activity")
subject_train<-read.table("./getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt",
                          col.names = "subject")

# read test and train data
features<-read.table("./getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/features.txt")
X_test<-read.table("./getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt")
X_train<-read.table("./getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt")
colnames(X_test)<-features[,2]
colnames(X_train)<-features[,2]

## grab columns only contain mean() or std()
message("Raw files read finished. Now start combining to one single data set.")
message("grabing only the variables relevant to std and mean, and assigning each with descriptive label")
BooLean_std<-sapply(features[,2], function(x) grepl('std()', x, fixed = TRUE))
BooLean_mean<-sapply(features[,2], function(x) grepl('mean()', x, fixed = TRUE))
X_test<-X_test[,BooLean_std|BooLean_mean]
X_train<-X_train[,BooLean_std|BooLean_mean]
colnames(X_test)<-gsub("-"," ", colnames(X_test))
colnames(X_test)<-gsub("\\()","",colnames(X_test))
colnames(X_train)<-gsub("-"," ", colnames(X_train))
colnames(X_train)<-gsub("\\()","",colnames(X_train))

## bind the train dataset and test dataset
Xtest<-cbind(subject_test,y_test,X_test)
Xtrain<-cbind(subject_train,y_train,X_train)
d_set<-rbind(Xtest,Xtrain)

## replace the activities code to character names
for (i in 1:nrow(activity_labels)){
  d_set$activity[d_set$activity==activity_labels[i,1]]<-as.character(activity_labels[i,2])
}
message("A dataset as required by Step 1-4 is created named d_set")

## creat an independent data set,
## take average of each variable for each activity and each subject

require("reshape2")
require("data.table")
dataset <- melt(d_set, id = c("activity", "subject"))
dataset <- dcast(dataset, subject + activity ~ variable, mean)

## write dataset to file in working directory
write.table(dataset, file = "./data.txt", row.name=FALSE)
dataset
message("A dataset required by Step 5 is created in name dataset and write into data.txt in R working directory.")

}


