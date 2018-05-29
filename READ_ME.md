#Code for run_analysis.R program


######Getting and Cleaning Data

######Course Project


setwd(".\\Desktop\\DataScienceclass\\GettingAndCleaningData\\UCI HAR Dataset")

getwd()
library(data.table)
library(dplyr)
library(tidyr)



###read in files


#####labels and descriptions
features<-read.table("features.txt")
features<-data.table(features)
l<-as.character(features$V2)



activity_labels<-read.table("activity_labels.txt")
activity_labels<-data.table(activity_labels)
names(activity_labels)<-c("activity_num","activity_label" )


#####Training Related Data

######data
features_train<-read.table(".\\Desktop\\DataScienceclass\\GettingAndCleaningData\\UCI HAR Dataset\\train\\X_train.txt")
features_train<-data.table(features_train)


######activity labels
activities_train<-read.table(".\\Desktop\\DataScienceclass\\GettingAndCleaningData\\UCI HAR Dataset\\train\\y_train.txt")
activities_train<-data.table(activities_train)
names(activities_train)<-"activities"


######subject number
subject_train<-read.table(".\\Desktop\\DataScienceclass\\GettingAndCleaningData\\UCI HAR Dataset\\train\\subject_train.txt")
subject_train<-data.table(subject_train)
names(subject_train)<-"subject"




######merge training data together
training_data<-cbind(subject_train, activities_train, features_train)


label_training_data<-merge(training_data, activity_labels, by.x = "activities", by.y = "activity_num" )

names(label_training_data)<-c("activities", "subject", l, "activity_label")


#####Test Related Data

features_test<-read.table(".\\Desktop\\DataScienceclass\\GettingAndCleaningData\\UCI HAR Dataset\\test\\X_test.txt")
features_test<-data.table(features_test)


######activity labels
activities_test<-read.table(".\\Desktop\\DataScienceclass\\GettingAndCleaningData\\UCI HAR Dataset\\test\\y_test.txt")
activities_test<-data.table(activities_test)

names(activities_test)<-"activities"


######subject number
subject_test<-read.table(".\\Desktop\\DataScienceclass\\GettingAndCleaningData\\UCI HAR Dataset\\test\\subject_test.txt")
subject_test<-data.table(subject_test)
names(subject_test)<-"subject"



#####merge test data together
test_data<-cbind(subject_test, activities_test, features_test)


label_test_data<-merge(test_data, activity_labels, by.x = "activities", by.y = "activity_num" )

names(label_test_data)<-c("activities", "subject", l, "activity_label")


rm(features_train, features_test,subject_test, subject_train, test_data, training_data)



###Question 1:  Merge training and test sets to create one data set.

Merged_train_test<-rbind(label_training_data,label_test_data)

rm(label_training_data, label_test_data)



###Question 2: Extract only the measurements on the mean and standard deviation for each measurement

M<-as.data.frame(Merged_train_test)
select_columns<-M[,c(2:1,564,grep("mean", colnames(M)),grep("std",colnames(M)))]



###Question 3: Use descriptive activity names to name the activities in the dataset

####I assume this means to change the columns names from V1, V2, etc. to the 
####appropriate labels provided in the datsets.
####This was done when test and training sets were being created.  See Lines ........... Above



###Question 4: Appropriately label the dataset with descriptive variable names.
####I assume this means to change the columns names from V1, V2, etc. to the 
####appropriate labels provided in the datsets.
####This was done when test and training sets were being created.  See Lines .............Above


###Question 5: From the data set in step 4, create a second, independent tidy data set with 
####the average of each variable for each activity and each subject.


ly.mean<-sapply(split(select_columns, list(select_columns$subject,select_columns$activity_label)),
                function(x) c(Mean = colMeans(x[,-c(1:3)])))

averages<-t(ly.mean)

averages<-data.frame(names = rownames(averages), averages, row.names = NULL)


library(matrixStats)
colSd <- function (x, na.rm=FALSE) apply(X=x, MARGIN=2, FUN=sd, na.rm=na.rm)

ly.std<-sapply(split(select_columns, list( select_columns$subject,select_columns$activity_label)),
                function(x) c(Std = colSd(x[,-c(1:3)])))



standard_dev<-t(ly.std)


standard_dev<-data.frame(names = rownames(standard_dev), standard_dev, row.names = NULL)


####Merge Mean and Standard Deviations by Subject.Activity combination


merged_mean_std<-merge(averages, standard_dev, by.x = "names", by.y = "names" )


####parse the names column into a subject column and activity column

names<-merged_mean_std$names

names<-as.data.frame(names) %>% separate(names, into = c("subject", "activity"), sep = "\\.")


merged_mean_std<-cbind(names,merged_mean_std)

merged_mean_std<-merged_mean_std[,c(1:2,4:161)]
subject<-as.numeric(merged_mean_std$subject)

TidyData<-cbind(subject,merged_mean_std[,2:160])


TidyData<-TidyData[order(TidyData$subject),]

####Write out the tidy data to a csv file

write.csv(TidyData, "TidyData.csv", row.names = FALSE)


