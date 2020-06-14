combineData <- function(){
  #read all data from the files
  features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
  activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
  subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
  x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
  y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
  subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
  x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
  y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")
 
  #combine the test and train data sets
  subjects <- rbind(subject_test,subject_train)
  X <- rbind(x_test,x_train)
  Y <- rbind(y_test,y_train)
  
  #converting the values into a single dataframe
  combined_data <- cbind(subjects,Y,X)
  names(combined_data)[2] <- "activity"
  
  #Descriptive activity labels
  combined_data$activity <- activities[combined_data$activity,2]
  
  #extracting only average,standard deviation of values
  required_data <- combined_data %>% select(subject,activity,contains("mean"),contains("std"))
  
  #Meaningful labels for variables
  names(required_data) <- gsub("acc","Accelerometer",names(required_data),ignore.case = T)
  names(required_data) <- gsub("gyro","Gyroscope",names(required_data),ignore.case = T)
  names(required_data) <- gsub("mag","Magnitude",names(required_data),ignore.case = T)
  names(required_data) <- gsub("-mean()","Mean",names(required_data),ignore.case = T)
  names(required_data) <- gsub("-std()","STD",names(required_data),ignore.case = T)
  names(required_data) <- gsub("freq","Frequency",names(required_data),ignore.case = T)
  names(required_data) <- gsub("gravity","Gravity",names(required_data),ignore.case = T)
  
  concise_data <- required_data %>% group_by(subject, activity) %>% summarise_all(list(mean))
  write.table(concise_data, "output.txt", row.name=FALSE)
  concise_data
  
}