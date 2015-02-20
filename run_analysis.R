#1. Merges the training and the test sets to create one data set.
tempTest<- read.table("test/X_test.txt")
tempTrain<- read.table("train/X_train.txt")
set <-rbind(tempTest,tempTrain)

tempTest<-read.table("test/y_test.txt")
tempTrain<- read.table("train//y_train.txt")
label <- rbind(tempTest,tempTrain)

tempTest <-read.table("test//subject_test.txt")
tempTrain<-read.table("train//subject_train.txt")
activity<-rbind(tempTest,tempTrain)

#2. Extracts only the measurements on the mean and standard deviation for each measurement. 
features <- read.table("features.txt")
#-mean includes also meanFreq, so we need to filter further
#index <- grep("-mean|-std", features[, 2])
index <- grep("-mean\\()|-std", features[, 2])
#check whether correct values are matched
merge(index,features,by.x="index",by.y="V1")

X <- set[, index$index]
names(X) <- features[index$index, 2]
names(X) <- gsub("\\(|\\)", "", names(X))#replace () with blank
names(X) <- tolower(names(X)) #change name all to lowercase

#3. Uses descriptive activity names to name the activities in the data set
activities <- read.table("activity_labels.txt")
activities[, 2] = gsub("_", " ", tolower(as.character(activities[, 2])))#change all to lowercase and replace _ with blank
label[,1] <- activities[label[,1], 2]
names(label)<-"activity"

#4. Appropriately labels the data set with descriptive variable names. 
names(activity) <- "subject"
cleaned <- cbind(set, label, activity)
write.table(cleaned, "merged_clean_data.txt")
#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

uniqueSubjects = unique(set)[,1]
numSubjects = length(unique(set)[,1])
numActivities = length(activities[,1])
numCols = dim(cleaned)[2]
result = cleaned[1:(numSubjects*numActivities), ]

row = 1
for (s in 1:5) {
  for (a in 1:numActivities) {
    result[row, 1] = uniqueSubjects[s]
    result[row, 2] = activities[a, 2]
    tmp <- cleaned[cleaned$subject==s & cleaned$activity==activities[a, 2], ]
    result[row, 3:(numCols-2)] <- colMeans(tmp[, 3:(numCols-2)])
    row = row+1
  }
}
write.table(result, "data_set_with_the_averages.txt")
for (s in 1:5)
  print(s)