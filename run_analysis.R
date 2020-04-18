getwd()
setwd("C:\\Users\\Mateusz\\Desktop\\DateScience\\3.DataCleaning\\project")

library(data.table)

# Download file

fileurl = 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'

if (!file.exists('./UCI_HAR_Dataset.zip')){
  download.file(fileurl,'./UCI_HAR_Dataset.zip', mode = 'wb')
  unzip("UCI_HAR_Dataset.zip", exdir = getwd())
}



#Read data
features <- read.csv('./UCI HAR Dataset/features.txt', header = FALSE, sep = ' ')
features <- as.character(features[,2])
train.x <- read.table('./UCI HAR Dataset/train/X_train.txt')
train.activity <- read.csv('./UCI HAR Dataset/train/y_train.txt', header = FALSE, sep = ' ')
train.subject <- read.csv('./UCI HAR Dataset/train/subject_train.txt',header = FALSE, sep = ' ')
data.train <-  data.frame(train.subject, train.activity, train.x)
names(data.train) <- c(c('subject', 'activity'), features)
test.x <- read.table('./UCI HAR Dataset/test/X_test.txt')
test.activity <- read.csv('./UCI HAR Dataset/test/y_test.txt', header = FALSE, sep = ' ')
test.subject <- read.csv('./UCI HAR Dataset/test/subject_test.txt', header = FALSE, sep = ' ')
data.test <-  data.frame(test.subject,test.activity,test.x)
names(data.test) <- c(c('subject', 'activity'), features)


# Merges the training and the test sets to create one data set.

test_and_train <- rbind(data.train, data.test)

#Extracts only the measurements on the mean and standard deviation for each measurement.

mean_and_std_measurement <- grep('mean|std', features) # get name values from the features
#mean_and_std_measurement
m_s_data <- test_and_train[,c(1,2,mean_and_std_measurement + 2)]  
#m_s_data

#Uses descriptive activity names to name the activities in the data set
#head(m_s_data$activity,200)
activity_desc <- read.table('./UCI HAR Dataset/activity_labels.txt', header = FALSE)
activity_desc <- as.character(activity_desc[,2])
m_s_data$activity <- activity_desc[m_s_data$activity]

#Appropriately labels the data set with descriptive variable names.
m_s_names <- gsub("[(][)]", "", names(m_s_data)) #remove () from names
m_s_names <- gsub("^t", "time", m_s_names)
m_s_names <- gsub("^f", "frequency", m_s_names)
names(m_s_data) <- m_s_names
names(m_s_data)

#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

m_s_data <- as.data.table(m_s_data)

m_s_d2<-aggregate(. ~subject + activity, m_s_data, mean)
m_s_d2<-m_s_d2[order(m_s_d2$subject,m_s_d2$activity),]
write.table(m_s_d2, file = "tidy_data.txt",row.name=FALSE)
