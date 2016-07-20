################################################################################

#Data Science Specialization, Coursera
#Getting and Cleaning Data Course Project 

# Cretaed by Renia Diamantopoulou
#19 July 2016

################################################################################

#Small description about the run_analysis.R script 

# This script will perform a set of commands in the direction of Cleaning and
# transforming the data downloaded from 
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

#1.Merges the training and the test sets to create one data set. 
#2.Extracts only the measurements on the mean and standard deviation for each measurement. 
#3.Uses descriptive activity names to name the activities in the data set 
#4.Appropriately labels the data set with descriptive variable names. 
#5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.



#We will be working in the directory that the UCI HAR Dataset was downloaded and unziped. 
# Set Directory
setwd("~/Desktop/coursera_datascience/GettingAndCleaningData/UCI HAR Dataset")

###############################################################################
# READ DATA
###############################################################################
# Read the train data:
features     = read.table('./features.txt')
activity_labels = read.table('./activity_labels.txt')
subject_train = read.table('./train/subject_train.txt')
x_train       = read.table('./train/x_train.txt')
y_train       = read.table('./train/y_train.txt')

# Read the test data:
subject_test = read.table('./test/subject_test.txt')
x_test = read.table('./test/x_test.txt')
y_test = read.table('./test/y_test.txt')



###############################################################################
# ASSIGN COLUMN NAMES
###############################################################################
# Assigin column names to the train data imported before
colnames(activity_labels)  = c('activity_id','activity_labels')
colnames(subject_train)  = "subject_id"
colnames(x_train)        = features[,2] 
colnames(y_train)        = "activity_id"

# Assign column names to the test data imported before
colnames(subject_test) = "subject_id"
colnames(x_test)  = features[,2] 
colnames(y_test)  = "activity_id"


###############################################################################
###############################################################################
# MERGE TRAINING AND TEST DATA TO CREATE THE FINAL DATA SET
###############################################################################
# Column Bind/Merge training data to create one data set
training_dataset = cbind(y_train,subject_train,x_train)

# Merge test data to create the final data test set
test_dataset = cbind(y_test,subject_test,x_test)

# Combine training and test data for the final data set
final_dataset = rbind(training_dataset,test_dataset)

colNames  = colnames(final_dataset); 

###############################################################################
###############################################################################
# Extract only the data on mean and standard deviation
###############################################################################

# CREATE COLUMNS VECTOR FROM THE final_dataset TO USE IT FOR 2. (MEAN & STDV)
logical_vector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));
final_dataset = final_dataset[logical_vector==TRUE]

# Use descriptive activity names to name the activities in the data set

# Merge the final_dataset with the acitivity_labels table to include descriptive activity names
final_dataset = merge(final_dataset,activity_labels,by='activity_id',all.x=TRUE)
colNames  = colnames(final_dataset)


###############################################################################
###############################################################################
###############################################################################
# Appropriately label the data set with descriptive variable names. 
# Cleaning up the variable names
for (i in 1:length(colNames)) 
{
        colNames[i] = gsub("\\()","",colNames[i])
        colNames[i] = gsub("-std$","StdDev",colNames[i])
        colNames[i] = gsub("-mean","Mean",colNames[i])
        colNames[i] = gsub("^(t)","time",colNames[i])
        colNames[i] = gsub("^(f)","freq",colNames[i])
        colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
        colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
        colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
        colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
        colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
        colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
        colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
};

# Reassigning the new descriptive column names to the finalData set
colnames(final_dataset) = colNames;

###############################################################################
###############################################################################
#Create a second, independent tidy data set with the average of each variable 
#for each activity and each subject. 
###############################################################################
###############################################################################

# Create a new table, without the activity_labels (TYPE) column
final_datasetNoActivityType  = final_dataset[,names(final_dataset) != 'activity_labels'];

# Summarizing the table to include just the mean of each variable for each activity and each subject
tidy_dataset    = aggregate(final_datasetNoActivityType[,names(final_datasetNoActivityType) != c('activity_id','subject_id')],by=list(activity_id=final_datasetNoActivityType$activity_id,subject_id = final_datasetNoActivityType$subject_id),FUN = mean , na.rm = TRUE);
# Merging the data to include descriptive acitvity names
tidy_dataset    = merge(tidy_dataset,activity_labels,by='activity_id',all.x=TRUE)

# Export the tidy_dataset 
write.table(tidy_dataset, './tidyData.txt',sep='\t')
