# Getting and Cleaning the data- Course 3- JHU
# Author Shivaprasad M
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


# LOading the packages, getting the data
pacman::p_load('data.table', 'reshape2')
wd <- getwd()
url <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
download.file(url = url, file.path(wd, 'projfiles.zip'))
unzip(zipfile = 'projfiles.zip')

# Activities and Features
activity_labels <- fread(file.path(wd, 'UCI HAR Dataset/activity_labels.txt'), 
                         col.names = c('Labels','activityNames'))
features <- fread(file.path(wd, 'UCI HAR Dataset/features.txt'),
                  col.names = c('index','featureNames'))
featuresreq <- grep("(mean|std)\\(\\)", features[, featureNames])
measurements <- features[featuresreq,featureNames]
measurements <- gsub('[()]', '', measurements)

# Load train datasets
train <- fread(file.path(wd,'UCI HAR Dataset/train/X_train.txt'))[, featuresreq, with=FALSE]
data.table::setnames(train, colnames(train), measurements)
train_activities <- fread(file.path(wd, 'UCI HAR Dataset/train/y_train.txt'),col.names = c('Activity'))
train_subjects <- fread(file.path(wd, 'UCI HAR Dataset/train/subject_train.txt'),col.names = c('SubjectNum'))
train <- cbind(train_subjects, train_activities,train)

# Load test datasets
test <- fread(file.path(wd,'UCI HAR Dataset/test/X_test.txt'))[, featuresreq, with=FALSE]
data.table::setnames(test, colnames(test), measurements)
test_activities <- fread(file.path(wd, 'UCI HAR Dataset/test/y_test.txt'),col.names = c('Activity'))
test_subjects <- fread(file.path(wd, 'UCI HAR Dataset/test/subject_test.txt'),col.names = c('SubjectNum'))
test <- cbind(test_subjects, test_activities,test)

# Combine (merge) datasets
combined <- rbind(train, test)

combined[['Activity']] <- factor(combined[,Activity], levels = activity_labels[['classLabels']],
                                 labels = activity_labels[['activityName']])

combined[['subjectNum']] <- as.factor(combined[,subjectNum])

combined <- reshape2::melt(data = combined, id = c('subjectNum','Activity'))
combined<- reshape2::dcast(data = combined, subjectName+ Activity ~ variable, fun.aggregate = mean)

data.table::fwrite(x =combined, file = 'tidy_data.txt', quote = FALSE)

