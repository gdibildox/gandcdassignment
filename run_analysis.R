# 1 - Merge the training and the test sets to create one data set.

subject <- read.table(file = "./UCI HAR Dataset/train/subject_train.txt")
x <- read.table(file = "./UCI HAR Dataset/train/X_train.txt")
y <- read.table(file = "./UCI HAR Dataset/train/y_train.txt")
train   <- cbind(subject, y, x)

subject <- read.table(file = "./UCI HAR Dataset/test/subject_test.txt")
x <- read.table(file = "./UCI HAR Dataset/test/X_test.txt")
y <- read.table(file = "./UCI HAR Dataset/test/y_test.txt")
test    <- cbind(subject, y, x)

dataset <- rbind(train,test)

# 2 - Extract only the measurements on the mean and standard deviation for each measurement. 
features <- read.table(file = "./UCI HAR Dataset/features.txt");
features <- features[,2]
match <- vector(mode="logical", length=length(features))
for (i in 1:length(features)){
  match[i]  <- !is.na(grep("mean|Mean|std|Std", as.character(features[i])) || 0)
}
subset_features <- features[match]
subset_data <- dataset[,match]
colnames(subset_data)[1:2] = c("ID","Activity")

# 3 Uses descriptive activity names to name the activities in the data set
activities <- c("WALKING", "WALKING_UP", "WALKING_DOWN", "SITTING", "STANDING", "LAYING")
subset_data$Activity <- factor(subset_data$Activity, labels = activities)

# 4 Appropriately labels the data set with descriptive variable names. 
subset_features <- gsub("-|\\(|\\)|\\,", ".", as.character(subset_features))
colnames(subset_data)[3:(length(subset_features)+2)] <- as.character(subset_features);

# 5 From the data set in step 4, creates a second, independent tidy data set with the average of 
# each variable for each activity and each subject.

if (file.exists("TidyDataset.txt")){
  file.remove("TidyDataset.txt")
}  
tidy_dataset <- aggregate(subset_data[, 3:dim(subset_data)[2]], list(subset_data$ID,subset_data$Activity),mean)
write.table(t(tidy_dataset),"TidyDataset.txt", row.name=FALSE)


# Write the feature names in a file
if (file.exists("Features.txt")){
  file.remove("Features.txt")
}  
write(x=t(paste(subset_features)), file="Features.txt", ncolumns = 1, append = TRUE, sep = " ") 