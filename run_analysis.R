# Course 3 Week 4 Project

# Set the working directory
setwd("/Users/joanmarius/Documents/Data Scientist Specialization/Getting And Cleaning Data/Week4/Project/UCI HAR Dataset")

## 1: Merge the training and the test sets to create one data set.

# 1.1 Read in files

# Reading train data
x_train <- read.table(file = "train/X_train.txt")
y_train <- read.table(file = "train/y_train.txt")
subject_train <- read.table(file = "train/subject_train.txt")

# Reading test data
x_test <- read.table(file = "test/X_test.txt")
y_test <- read.table(file = "test/y_test.txt")
subject_test <- read.table(file = "test/subject_test.txt")

# Reading feature data
feature <- read.table(file = "features.txt")

# Reading activity labels
activity_labels <- read.table(file = "activity_labels.txt")

# 1.2 Add column names
View(feature)
View(subject_train)
View(activity_labels)
dim(feature)
dim(x_train)

# train data
names(x_train) <- feature[,2]
names(y_train) <- "activityid"
names(subject_train) <- "subjectid"

# test data
names(x_test) <- feature[,2]
names(y_test) <- "activityid"
names(subject_test) <- "subjectid"

# activity label
names(activity_labels) <- c("activityid", "activitylabel")

# 1.3 Merge the data sets

# Creater a factor column
train <- cbind(x_train, y_train, subject_train)
test <- cbind(x_test, y_test, subject_test)
group <- c(rep("train", nrow(train)), rep("test", nrow(test)))
stack <- rbind(train, test)
mrg_x <- cbind(group, stack)

# 2: Extract only the measurements on the mean and standard deviation for each measurement.

# 2.1 Create a colname vector
colname <- names(mrg_x)

# 2.2 Identify the columans we would like to keep
mean_std <- (grepl("group", colname) | 
                     grepl("activityid", colname) | 
                     grepl("activitylable", colname) |
                     grepl("mean", colname) |
                     grepl("std", colname) |
                     grepl("subjectid", colname))

# Remove uneccesary columns
mgr_subset <- mrg_x[,which(mean_std == TRUE)]

# 3: Uses descriptive activity names to name the activities in the data set

mgr_act <- merge(mgr_subset, activity_labels, by = "activityid", all.x = TRUE)
names(mgr_act[length(mgr_act)])
View(head(mgr_act, n=4))

# 4:Appropriately labels the data set with descriptive variable names.
# In set 2

# 5: From the data set in step 4, creates a second, independent tidy data set 
# with the average of each variable for each activity and each subject.

# 5.1 Set subjectid as a factor
mgr_act$subjectid <- as.factor(mgr_act$subjectid)
tidyset <- data.table::data.table(mgr_act)
tidydata <- aggregate(. ~subjectid + activitylabel, mgr_act, mean)
tidydata <- tidydata[order(tidydata$subjectid,tidydata$activitylabel),]
write.table(tidydata, file = "Tidy.txt", row.names = FALSE) # Save as a txt file
