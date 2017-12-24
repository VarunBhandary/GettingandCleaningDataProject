##       The run_analysis.R does the following.
##       1. Merges the training and the test sets to create one data set.
##       2. Extracts only the measurements on the mean and standard deviation 
##          for each measurement.
##       3. Uses descriptive activity names to name the activities in the data 
##          set
##       4. Appropriately labels the data set with descriptive variable names.
##       5. From the data set in step 4, creates a second, independent tidy data
##          set with the average of each variable for each activity and each 
##          subject.

##       Downlading the Data Sets
##       Reading individual data sets from the downloaded data set


#       Download the zipped data file from the URL and save the contents at a specific location

DataZipUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
DataZipFileName <- "UCI HAR Dataset.zip"

if (!file.exists(DataZipFileName)) {
        download.file(DataZipUrl, DataZipFileName, mode = "wb")
}

dataPath <- "UCI HAR Dataset"
if (!file.exists(dataPath)) {
        unzip(DataZipFileName)
}


# read training data
trainingSubjects <- read.table(file.path(dataPath, "train", "subject_train.txt"))

trainingValues <- read.table(file.path(dataPath, "train", "X_train.txt"))

trainingActivity <- read.table(file.path(dataPath, "train", "y_train.txt"))


# read test data
testSubjects <- read.table(file.path(dataPath, "test", "subject_test.txt"))

testValues <- read.table(file.path(dataPath, "test", "X_test.txt"))

testActivity <- read.table(file.path(dataPath, "test", "y_test.txt"))


# read features
# reading the table with as.is as TRUE does not don't convert text labels to factors
features <- read.table(file.path(dataPath, "features.txt"), as.is = TRUE)

# read activity labels
activities <- read.table(file.path(dataPath, "activity_labels.txt"))

names(activities) <- c("ActivityID", "ActivityText")


## Step 1 -  Merging the Data Sets


ActivityTotalData <- rbind(cbind(trainingActivity, trainingSubjects, trainingValues)
                           ,cbind(testActivity, testSubjects, testValues))


rm(trainingSubjects, trainingValues, trainingActivity, testSubjects, testValues, 
   testActivity)

colnames(ActivityTotalData) <- c("Activity", "Subject", features[, 2])

## Step 2 - Extract only the measurements on the mean and standard deviation
##          for each measurement

retainCols <- grepl("Subject|Activity|mean|std", colnames(ActivityTotalData))


ActivityTotalData <- ActivityTotalData[,retainCols]

## Step 3 - Use descriptive activity names to name the activities in the data
##          set

ActivityTotalData$Activity <- factor(ActivityTotalData$Activity, 
                                     levels = activities[, 1], labels = activities[, 2])

## Step 4 - Appropriately labels the data set with descriptive variable names

#Clean up existing column names
ActivityTotalDataColumnNames <- colnames(ActivityTotalData)
ActivityTotalDataColumnNames <- gsub("[\\(\\)-]", "", ActivityTotalDataColumnNames)
ActivityTotalDataColumnNames <- gsub("^f", "frequencyDomain", ActivityTotalDataColumnNames)
ActivityTotalDataColumnNames <- gsub("^t", "timeDomain", ActivityTotalDataColumnNames)
ActivityTotalDataColumnNames <- gsub("Acc", "Accelerometer", ActivityTotalDataColumnNames)
ActivityTotalDataColumnNames <- gsub("Gyro", "Gyroscope", ActivityTotalDataColumnNames)
ActivityTotalDataColumnNames <- gsub("Mag", "Magnitude", ActivityTotalDataColumnNames)
ActivityTotalDataColumnNames <- gsub("Freq", "Frequency", ActivityTotalDataColumnNames)
ActivityTotalDataColumnNames <- gsub("mean", "Mean", ActivityTotalDataColumnNames)
ActivityTotalDataColumnNames <- gsub("std", "StandardDeviation", ActivityTotalDataColumnNames)

#set new column names
colnames(ActivityTotalData) <- ActivityTotalDataColumnNames

# Step 5 - Create a second, independent tidy set with the average of each
#          variable for each activity and each subject

ActivityMean_Activity_Sub <- ActivityTotalData %>% group_by(Subject, Activity) %>% summarise_all(funs(mean))

#Writing data table in txt file for submission
write.table(ActivityMean_Activity_Sub, file = "ActivityMean_TidyOutput.txt", row.names = FALSE)

