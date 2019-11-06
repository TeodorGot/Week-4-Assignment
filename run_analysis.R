library(dplyr)
#in github I removed my path, so insert yours
pathdata <- file.path("You path here")
files <- list.files(pathdata, recursive=TRUE)
# read train data
xtrain <- read.table(file.path(pathdata, "train", "X_train.txt"))
ytrain <- read.table(file.path(pathdata, "train", "y_train.txt"))
sub_train <- read.table(file.path(pathdata, "train", "subject_train.txt"))
# read test data
xtest <- read.table(file.path(pathdata, "test", "X_test.txt"))
ytest <- read.table(file.path(pathdata, "test", "y_test.txt"))
sub_test = read.table(file.path(pathdata, "test", "subject_test.txt"))

# read features data
features <- read.table(file.path(pathdata, "features.txt"))

# read activity labels
activity_labels <- read.table(file.path(pathdata, "activity_labels.txt"))

#create column values to the Train Data
colnames(xtrain) <- features[,2]
colnames(ytrain) <- "activityId"
colnames(sub_train) <- "subjectId"

#create column values to the test data
colnames(xtest) <- features[,2]
colnames(ytest) <- "activityId"
colnames(sub_test) <- "subjectId"

#Check for the activity labels value
colnames(activity_labels) <- c('activityId','activityType')

#1. Merges the training and the test sets to create one data set.
mrg_train <- cbind(ytrain, sub_train, xtrain)
mrg_test <- cbind(ytest, sub_test, xtest)
sub_total <- rbind(mrg_train, mrg_test)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

col_names <- colnames(sub_total) # check all the values that are available
mean_and_std <- (grepl("activityId" , col_names) 
                | grepl("subjectId" , col_names) 
                | grepl("mean.." , col_names) 
                | grepl("std.." , col_names))

#A subtset has to be created to get the required dataset
set_for_mean_and_std <- sub_total[ , mean_and_std == TRUE]

#3. Use descriptive activity names to name the activities in the data set
set_activity_names <- merge(set_for_mean_and_std, activity_labels, 
                             by="activityId", all.x=TRUE)

#4. Appropriately labels the data set with descriptive variable names.
#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
tidy_set <- aggregate(. ~subjectId + activityId, set_activity_names, mean)
tidy_set <- tidy_set[order(tidy_set$subjectId, tidy_set$activityId),]


write.table(tidy_set, "TidySet.txt", row.name=FALSE)


