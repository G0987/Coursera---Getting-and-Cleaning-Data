install.packages("reshape2")
library(reshape2)

library(plyr)

## 1.Merges the training and the test sets to create one data set.

## 1.1 Read and merge the activity data
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
X_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
X_merge <- rbind(X_test, X_train)

## 1.2 Read and merge the activity labels
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")
y_merge <- rbind(y_test, y_train)

## 1.3 Read and merge the subject labels
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
subject_merge <- rbind(subject_test, subject_train)

data_v1 <- cbind(X_merge, y_merge, subject_merge)
data <- data_v1 ## for testing only

## 2. Extracts only the measurements on the mean and standard deviation for each measurement.

## 2.1 Read the column labels
features <- read.table("./UCI HAR Dataset/features.txt", stringsAsFactors=FALSE)

## 2.2 Extract the columns that have mean or standard deviation
## cool variable name, ain't it :)
mean_std <- grep(".*mean().*|.*std().*", features[,2])
features <- features[mean_std,]

mean_std <- c(mean_std, 562, 563)

data <- data[, mean_std]

## 3. Uses descriptive activity names to name the activities in the data set
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")
c = 1
while (c < nrow(data)) {
    data[c, 80] <- gsub(activity_labels[data[c, 80], 1], activity_labels[data[c, 80], 2], data[c, 80])
    c <- c + 1
}

## 4. Appropriately labels the data set with descriptive variable names
colnames(data) <- c(features[,2], "activity", "subject")

## 5. From the data set in step 4, creates a second, independent tidy data set 
##    with the average of each variable for each activity and each subject.

## 5.1 Melt the data and recast it based on subject + activity primary key
molten_data = melt(data, id = c("activity", "subject"))
tidy_data = dcast(molten_data, subject + activity ~ variable, mean)

## 5.2 Print the data as a table and csv file 
write.table(tidy_data, file = "./tidy_data.txt", row.name=FALSE)
write.csv(tidy_data, file = "./tidy_data.csv")
