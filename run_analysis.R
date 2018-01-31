#-------------------------------------------#
#Setup the Rstudio environment with necessary packages

install.packages("tidyr")
install.packages("dplyr")
install.packages("plyr")
install.packages("ggplot2")
library(tidyr)
library(dplyr)
library(plyr)
library(ggplot2)

#Setup complete

#-------------------------------------------#
# Download and unzip file

setwd("~/Documents/Coursera/GettingAndCleaningData")
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, "week4assignmentdataset.zip")
list.files()
unzip("week4assignmentdataset.zip")
list.files()

# Download and unzip complete

#-------------------------------------------#
# Read the features.txt file so that we have a readily available character vector 
# of descriptive variable names to use for step 4 of the assignment. 

setwd("~/Documents/Coursera/GettingAndCleaningData/UCI HAR Dataset")
features <- read.table("features.txt")
View(features)
features <- features %>% mutate(DescriptiveNames = gsub("\\()", "", V2))
features <- features %>% mutate(DescriptiveNames = gsub("\\-", "_", DescriptiveNames))
features <- features %>% mutate(DescriptiveNames = gsub("\\,", "_", DescriptiveNames))
features <- features %>% mutate(DescriptiveNames = gsub("\\(", "_", DescriptiveNames))
features <- features %>% mutate(DescriptiveNames = gsub("\\)", "_", DescriptiveNames))
features <- features %>% mutate(DescriptiveNames = gsub("\\_$", "", DescriptiveNames))
features <- features %>% mutate(DescriptiveNames = gsub("^t", "time_", DescriptiveNames))
features <- features %>% mutate(DescriptiveNames = gsub("^f", "freq_", DescriptiveNames))
View(features)
feature_names <- as.vector(features$DescriptiveNames)
str(feature_names)

# Read features complete

#-------------------------------------------#

# Read the activities file so that it can be used later in step 3 to assign 
# descriptive names to activities of the subjects.

activities <- read.table("activity_labels.txt", col.names = c("ActivityLabel", "ActivityName"))

# Read activities complete

#-------------------------------------------#

# Read files from the test dataset directory

setwd("~/Documents/Coursera/GettingAndCleaningData/UCI HAR Dataset/test")
subject_test <- read.table("subject_test.txt", col.names = c("SubjectId"))
dim(subject_test)
# Use the feature_names to rename the column names for x_test
x_test <- read.table("X_test.txt", col.names = feature_names)   
dim(x_test)
y_test <- read.table("y_test.txt", col.names = c("ActivityLabel"))
dim(y_test)

# Read files from the test dataset directory complete

#-------------------------------------------#

# Merge all the dataframes relevant to test dataset to create a single dataset
# that contains subjects, their activities and the measurements associated to their activities

subject_x_test <- cbind(subject_test, x_test)
dim(subject_x_test)
subject_x_y_test <- cbind(subject_x_test, y_test)
dim(subject_x_y_test)
# Add a new column to indicate it is a "Test" dataset.
subject_x_y_test <- subject_x_y_test %>% mutate(DataType = "Test")
dim(subject_x_y_test)
View(subject_x_y_test)

# Merge complete and now we have a single dataframe containing the complete test dataset

#-------------------------------------------#

# Read files from the train dataset directory

setwd("~/Documents/Coursera/GettingAndCleaningData/UCI HAR Dataset/train")
subject_train <- read.table("subject_train.txt", col.names = c("SubjectId"))
dim(subject_train)
# Use the feature_names to rename the column names for x_train
x_train <- read.table("X_train.txt", col.names = feature_names)   
dim(x_train)
y_train <- read.table("y_train.txt", col.names = c("ActivityLabel"))
dim(y_train)

# Read files from the train dataset directory complete

#-------------------------------------------#

# Merge all the dataframes relevant to train dataset to create a single dataset
# that contains subjects, their activities and the measurements associated to their activities

subject_x_train <- cbind(subject_train, x_train)
dim(subject_x_train)
subject_x_y_train <- cbind(subject_x_train, y_train)
dim(subject_x_y_train)
# Add a new column to indicate it is a "Train" dataset.
subject_x_y_train <- subject_x_y_train %>% mutate(DataType = "Train")
dim(subject_x_y_train)
View(subject_x_y_train)

# Merge complete and now we have a single dataframe containing the complete "Train" dataset

#-------------------------------------------#

# Merge the "Test" and "Train" datasets by appending the rows because the columns are 
# in the same order in both dataframes. This is 

alldata <- rbind(subject_x_y_train, subject_x_y_test)
dim(alldata)
View(alldata)
alldata <- alldata %>% select(SubjectId, ActivityLabel, DataType, everything())
View(alldata)

# Merge complete and now we have a single dataframe containing the 
# complete "Test" and "Train" datasets - End of Step 1

#-------------------------------------------#

#Extract columns that represent mean and std deviations of measurements

relevantdata <- select(alldata, SubjectId, ActivityLabel, 
                       contains("mean"), contains("std"))
dim(relevantdata)
View(relevantdata)

# Extraction of columns that contain mean and standard deviation complete. End of Step 2.

#-------------------------------------------#

# Using the existing dataframe activities that contains descriptive activity names,
# join with relevant data to produce a combined dataset with activity names.

View(activities)
relevantdata_names <- join(relevantdata, activities, by = "ActivityLabel")
dim(relevantdata_names)
relevantdata_names <- select(relevantdata_names, SubjectId, ActivityLabel, ActivityName, everything())
View(relevantdata_names)

step3_tidydata <- relevantdata_names
step4_tidydata <- relevantdata_names

# Now we have a dataset complete with descriptive active names. Step 3 completed.
# Also since we had already made the variable names descriptive when extracting the files, 
# step 4 is also completed.

#-------------------------------------------#

# In this step we compute the mean for each measurement column for each subject-Activity group

step5_tidydata <- relevantdata_names %>% group_by(SubjectId, ActivityLabel, ActivityName) %>% summarise_all(funs(avg = mean), na.rm = TRUE)
View(step5_tidydata)
setwd("~/Documents/Coursera/GettingAndCleaningData/UCI HAR Dataset")
write.table(step5_tidydata, "step5_tidydata.txt", row.names = FALSE)
# step 5 is completed.

#-------------------------------------------#
