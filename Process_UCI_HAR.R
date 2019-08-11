library(dplyr)
library(magrittr)
library(readr)

if(!grepl("UCI HAR Dataset",getwd())){
  setwd("UCI HAR Dataset")
}

#Import all required column names and metadata from the respective files
activity_labels <- read.delim(header = FALSE, file = "activity_labels.txt", sep = " ", col.names = c("code", "activity"))
features <- read.delim(header = FALSE, file = "features.txt", sep = " ", col.names = c("num", "feature"))
features_info <- read.delim(header = FALSE, file = "features_info.txt", sep = " ")

#import test data files
X_test <- read_table2("test/X_test.txt", col_names = FALSE)
testy <- read.delim(header = FALSE, file = "test/y_test.txt", sep = " ", col.names = "code")
subject_test <- read.delim(header = FALSE, file = "test/subject_test.txt", sep = " ", col.names = "subject")

#import train data files
X_train <- read_table2("train/X_train.txt", col_names = FALSE)
trainy <- read.delim(header = FALSE, file = "train/y_train.txt", sep = " ", col.names = "code")
subject_train <- read.delim(header = FALSE, file = "train/subject_train.txt", sep = " ", col.names = "subject")

#combine the two data sets
combo <- rbind(X_train, X_test)

#combine the subjects
subjects <- rbind(subject_train, subject_test)

#combine the labels
labels <- rbind(trainy, testy)

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

colnames(combo) <- features$feature

combo <- combo[,!duplicated(colnames(combo), fromLast = TRUE)]

#isolate mean and std
#"Extracts only the measurements on the mean and standard deviation for each measurement."
combo_mean_std <- combo %>% select(contains("Mean-"), contains("STD"))

labels_activities <- left_join(labels, activity_labels)

tidysheet <- cbind(subjects$subject, labels_activities$activity, combo_mean_std)

colnames(tidysheet)[1:2] <- c("subject", "activity")


#From the data set in step 4, creates a second, independent tidy data set with
#the average of each variable for each activity and each subject.
tidy_summary <- tidysheet %>% group_by(subject, activity) %>% summarise_if(.predicate = is.numeric, mean)

