## download the files and save the folder
## the data is now in the UCI HAR folder in C:/Users/write/Documents/UCI HAR Dataset
## Loading the packages for the project
library(base)
library(dplyr)
library(data.table)
##we have to now read all the files in the two data sets
## read the subject_test file and store it in sub_test
sub_test <- read.table("C:/Users/write/Documents/UCI HAR Dataset/test/subject_test.txt")
## read the X_test file and store it in x_test
x_test <- read.table("C:/Users/write/Documents/UCI HAR Dataset/test/X_test.txt")
## read the y_test file and store it in y_test
y_test <- read.table("C:/Users/write/Documents/UCI HAR Dataset/test/y_test.txt")
## Now we will read files from the train folder
sub_train <- read.table("C:/Users/write/Documents/UCI HAR Dataset/train/subject_train.txt")
## similarly we will read the files X_train and y_train into R
x_train <- read.table("C:/Users/write/Documents/UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("C:/Users/write/Documents/UCI HAR Dataset/train/y_train.txt")
## Now we will read the two files - features and activity_labels into R
features <- read.table("C:/Users/write/Documents/UCI HAR Dataset/features.txt")
activity_labels <- read.table("C:/Users/write/Documents/UCI HAR Dataset/activity_labels.txt")
## 1 to merge the files in the test and train 

xall <- rbind(x_test, x_train)
yall <- rbind(y_test, y_train)
sub_all <- rbind(sub_test, sub_train)
## to merge all together
all_files <- cbind(sub_all, yall, xall)

## giving descriptive activity names 
names_features <- as.character(features[,2])
columns_des <- c("subject", "activity", names_features)
colnames(all_files) <- columns_des

##2 extract only the measurements on the mean and standard deviation for each measurement
## to extract only means
meansOnly <- grep("mean()", colnames(all_files))
## to extract only standard deviation 
stddevOnly <- grep("std()",colnames(all_files))

## to add this back to the data
new_cols <- c(meansOnly, stddevOnly)
sorted_new_cols <- sort(new_cols)
extracted_data <- all_files[, c(1, 2, sorted_new_cols)]
##to get rid of the mean frequency column in the extracted_data
extracted_data2 <- extracted_data[, !grepl("Freq", colnames(extracted_data))]
## get rid of the mean frequency
names(extracted_data)
## let us now set activity and subject variables as factors

extracted_data$activity <- as.factor(extracted_data$activity)
extracted_data$subject <- as.factor(extracted_data$subject)

## Appropriately labels the data set with descriptive variable names. 


## get tidy data
tidy1 <- aggregate(.~subject + activity, extracted_data2, mean)

colnames(tidy1) <- colnames(extracted_data2)

levels(tidy2[,2]) <- c('walk', 'upstairswalk', 'downstairswalk', 'sit', 'stand', 'lay')

tidy2 <- tidy1

names(tidy2)<-gsub("Acc", "Accelerometer", names(tidy2))
names(tidy2)<-gsub("BodyBody", "Body", names(tidy2))
names(tidy2)<-gsub("mean()", "Mean", names(tidy2))
names(tidy2)<-gsub("std()", "Std.Deviation", names(tidy2))
## writing into a text file
write.table(tidy2, "Samsung_Data1.txt", sep = "")

