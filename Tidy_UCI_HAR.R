library(dplyr)
library(magrittr)
library(readr)

################################################################################
################################################################################
#
#                        Downloading and unzipping files
#
################################################################################
################################################################################
## Unhash below for downloading and unzipping files
#
#
#setwd("~/Dropbox/Doctorate/Coursera_data_science/Coursera_R/Week4/Project/Project_unzip/UCI HAR Dataset/")
#      
#URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
#
#if(!dir.exists("Week4")) {
#  dir.create("Week4")
#}
#
#if(!dir.exists("Week4/Project")) {
#  dir.create("Week4/Project")
#}
#
#if(!dir.exists("Week4/Project/UCI HAR Dataset")) { 
#  download.file(url = URL, method = "curl",
#                destfile = "./Week4/Project/Dataset.zip")
#  
#  unzip(zipfile = "./Week4/Project/Dataset.zip",
#        exdir = "Week4/Project/UCI HAR Dataset")
#}
#
##Set workind directory to "UCI HAR Dataset"
#if(!grepl("UCI HAR Dataset",getwd())){
#  setwd("UCI HAR Dataset")
#}
#
################################################################################
################################################################################


#Import all required column names and metadata from the respective files
activity_labels <- read.delim(header = FALSE, file = "activity_labels.txt", sep = " ", col.names = c("code", "activity"))
features <- read.delim(header = FALSE, file = "features.txt", sep = " ", col.names = c("num", "feature"))

#import test data files
X_test <- read_table2("test/X_test.txt", col_names = FALSE)
y_test <- read.delim(header = FALSE, file = "test/y_test.txt", sep = " ", col.names = "code")
subject_test <- read.delim(header = FALSE, file = "test/subject_test.txt", sep = " ", col.names = "subject")

#import train data files
X_train <- read_table2("train/X_train.txt", col_names = FALSE)
y_train <- read.delim(header = FALSE, file = "train/y_train.txt", sep = " ", col.names = "code")
subject_train <- read.delim(header = FALSE, file = "train/subject_train.txt", sep = " ", col.names = "subject")

#combine the two data sets
combined_data <- rbind(X_train, X_test)

#combine the subjects
combined_subjects <- rbind(subject_train, subject_test)

#combine the labels
combined_labels <- rbind(y_train, y_test)

#rename columns of both train and test:
#"Appropriately labels the data set with descriptive variable names"
features$feature <- gsub("BodyBody", "Body", features$feature)
features$feature <- gsub("^fBody", "FourierTransformedBody", features$feature)
features$feature <- gsub("tBody", "TimeBody", features$feature)
features$feature <- gsub("tGravity", "TimeGravity", features$feature)
features$feature <- gsub("Acc", "Acceleration", features$feature)
features$feature <- gsub("Gyro", "Gyroscrope", features$feature)
features$feature <- gsub("Mag", "Magnitude", features$feature)
features$feature <- gsub("-mean\\(\\)", "Mean", features$feature)
features$feature <- gsub("-std\\(\\)", "STD", features$feature)

colnames(combined_data) <- features$feature

combined_data <- combined_data[,!duplicated(colnames(combined_data), fromLast = TRUE)]

#isolate mean and std
#"Extracts only the measurements on the mean and standard deviation for each measurement."
combined_data_mean_std <- combined_data %>% select(contains("Mean"), contains("STD"))

#Join the labels and activity labels to provide descriptive names rather than
#codes.
labels_activities <- left_join(combined_labels, activity_labels)

#Bind the subject, activity and mean/std data to a single data frame
tidysheet <- cbind(combined_subjects$subject, labels_activities$activity, combined_data_mean_std)

#Change column names to be more descriptive
colnames(tidysheet)[1:2] <- c("subject", "activity")


#From the data set in step 4, creates a second, independent tidy data set with
#the average of each variable for each activity and each subject.
tidy_summary <- tidysheet %>%
  group_by(subject, activity) %>%
  summarise_if(.predicate = is.numeric, mean)

write.table(x = tidy_summary, file = "tidy_summary.txt", row.names = FALSE)
