
## 1. Merges the training and the test sets to create one data set.

# Read data

features <- read.table("features.txt")
activity_labels <- read.table("activity_labels.txt") 

subjectTrain <- read.table("subject_train.txt")
xTrain <- read.table("X_train.txt")
yTrain <- read.table("y_train.txt")

# Rename column names
colnames(activity_labels) <- c("activity_ID", "activity_Type")
colnames(subjectTrain)[1] <- "Subject_ID"
colnames(xTrain) <- features[,2]
colnames(yTrain) <- "activity_ID"

#  Combine in one data
trainingData <- cbind(yTrain,subjectTrain,xTrain)

# Read test_data
subjectTest <- read.table("subject_test.txt")
xTest <- read.table("X_test.txt")
yTest <- read.table("y_test.txt")

# Rename column names
colnames(subjectTest)[1] <- "Subject_ID"
colnames(xTest) <- features[,2]
colnames(yTest) <- "activity_ID"

# Combine in one data
testData <- cbind(yTest,subjectTest,xTest)


# Combine training and test data to create a final data set
finalData <-  rbind(trainingData,testData)

## 2. Extracts only the measurements on the mean and standard deviation for each measurement.

# Extracts only the measurements on mean and std in train and test data
mean_and_std <- grep("-(mean|std)\\(\\)", features[, 2])
xTest1 <- xTest[, mean_and_std]
xTrain1<- xTrain[, mean_and_std]

# Create final Data with mean and std (only)
trainingData1 <- cbind(yTrain,subjectTrain,xTrain1)
testData1 <- cbind(yTest,subjectTest,xTest1)
finalData1 <-  rbind(trainingData1,testData1)

## 3. Uses descriptive activity names to name the activities in the data set

finalData <- merge(finalData,activity_labels,by="activity_ID",all.x=TRUE)

## 4. Appropriately labels the data set with descriptive variable names.

colNames  <- colnames(finalData)

for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
}


## 5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

colnames(finalData) <- colNames

finalData$activity_ID <- as.factor(finalData$activity_ID)
finalData$Subject_ID <- as.factor(finalData$Subject_ID)
tidyData <- aggregate(finalData, by=list(finalData$activity_ID, finalData$Subject_ID), FUN = "mean")
tidyData <- tidyData[,-c(3,4,566)]
colnames(tidyData)[1] <-"activity_ID"
colnames(tidyData)[2] <-"Subject_ID"
tidyData <- merge(tidyData,activity_labels,by="activity_ID",all.x=TRUE)

