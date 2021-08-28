#Loading dplyr package
library(dplyr)

#Downloading the dataset and naming it Coursera_Final.zip
filename <- "Coursera_Final.zip"
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileURL, filename, method="curl")

#Unzipping the downloaded .zip file which creates a new folder
#names UCI HAR Dataset.
unzip(filename)


#Assigning variable names to all data frames along with
#giving them their respective column names
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")



#Step1. Merging the training and the test  sets
#to create one single data set.
X <- rbind(x_train, x_test)
Y <- rbind(y_train, y_test)
Subject <- rbind(subject_train, subject_test)
Merged_Data <- cbind(Subject, Y, X)


#Step2. Extracting only the measurements on
#the mean and standard deviation for each measurement.
tidyData <- Merged_Data %>% select(subject, code, contains("mean"), contains("std"))


#Step3. Using descriptive activity names to name the 
#activities in the data set.
tidyData$code <- activities[tidyData$code, 2]


#Step4. Appropriately labeling the data set 
#with descriptive variable names.
names(tidyData)[2] = "activity"
names(tidyData)<-gsub("Acc", "Accelerometer", names(tidyData))
names(tidyData)<-gsub("Gyro", "Gyroscope", names(tidyData))
names(tidyData)<-gsub("BodyBody", "Body", names(tidyData))
names(tidyData)<-gsub("Mag", "Magnitude", names(tidyData))
names(tidyData)<-gsub("^t", "Time", names(tidyData))
names(tidyData)<-gsub("^f", "Frequency", names(tidyData))
names(tidyData)<-gsub("tBody", "TimeBody", names(tidyData))
names(tidyData)<-gsub("-mean()", "Mean", names(tidyData), ignore.case = TRUE)
names(tidyData)<-gsub("-std()", "STD", names(tidyData), ignore.case = TRUE)
names(tidyData)<-gsub("-freq()", "Frequency", names(tidyData), ignore.case = TRUE)
names(tidyData)<-gsub("angle", "Angle", names(tidyData))
names(tidyData)<-gsub("gravity", "Gravity", names(tidyData))


#Step5 From the data set in step 4, a second,
#independent tidy data set with the average of each 
#variable for each activity and each subject was 
#created.

finalData <- tidyData %>%
        group_by(subject, activity) %>%
        summarise_all(funs(mean))
write.table(finalData, "finalData.txt", row.name=FALSE)



                        
                                                         