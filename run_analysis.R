# for pipe operations and cleaning data we need "dplyr" package
library(dplyr)

# download and unzip data from directory
dataset <- "getdata_projectfiles_UCI HAR Dataset.zip"
if(!file.exists("UCI HAR Dataset")) {
  unzip(datatset)
  }
# create objects include data
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n", "functions"))
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")

# Merges the training and the test sets to create one data set.
X <- rbind(x_train, x_test)
Y <- rbind(y_train, y_test)
Subject <- rbind(subject_train, subject_test)
Merged_Data <- cbind(Subject, Y, X)

# Extracts only the measurements on the mean and standard deviation for each measurement.
tidy_data_1 <- Merged_Data %>% 
  select(subject, code, contains("mean"), contains("std"))

# Uses descriptive activity names to name the activities in the data set
tidy_data_1$code <- activities[tidy_data_1$code, 2]

# Appropriately labels the data set with descriptive variable names.
names(tidy_data_1)[2] = "activity"
names(tidy_data_1)<-gsub("Acc", "Accelerometer", names(tidy_data_1))
names(tidy_data_1)<-gsub("Gyro", "Gyroscope", names(tidy_data_1))
names(tidy_data_1)<-gsub("BodyBody", "Body", names(tidy_data_1))
names(tidy_data_1)<-gsub("Mag", "Magnitude", names(tidy_data_1))
names(tidy_data_1)<-gsub("^t", "Time", names(tidy_data_1))
names(tidy_data_1)<-gsub("^f", "Frequency", names(tidy_data_1))
names(tidy_data_1)<-gsub("tBody", "Time_Body", names(tidy_data_1))
names(tidy_data_1)<-gsub("-mean()", "Mean", names(tidy_data_1), ignore.case = TRUE)
names(tidy_data_1)<-gsub("-std()", "STD", names(tidy_data_1), ignore.case = TRUE)
names(tidy_data_1)<-gsub("-freq()", "Frequency", names(tidy_data_1), ignore.case = TRUE)
names(tidy_data_1)<-gsub("angle", "Angle", names(tidy_data_1))
names(tidy_data_1)<-gsub("gravity", "Gravity", names(tidy_data_1))

# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subjec
tidy_data_2 <- tidy_data_1 %>%
  group_by(subject, activity) %>%
  summarise_all(funs(mean))
write.table(tidy_data_2, "Tidy_data.txt", row.name=FALSE)
str(tidy_data_2)
View(tidy_data_2)