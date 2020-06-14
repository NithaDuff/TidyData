combineData <- function(fileName ="~/Duff/TidyData/UCI HAR Dataset"){
  #read all data from the files
  setwd(fileName)
  test_data <- read.csv("test/X_test.txt",header = F)
  tst_sub <- read.csv("test/subject_test.txt",header = F)
  act_tst <- read.csv("test/y_test.txt",header = F)
  
  train_data <- read.csv("train/X_train.txt",header = F)
  trn_sub <- read.csv("train/subject_train.txt",header = F)
  act_trn <- read.csv("train/y_train.txt",header = F)
  
  act_lbl <- read.csv("activity_labels.txt",header = F,sep = " ") 
  fileName <- "~/Duff/TidyData/"
  setwd(fileName)
  #combine the test and train data sets
  subjects <- rbind(tst_sub,trn_sub)
  rm(tst_sub)
  rm(trn_sub)
  act <- rbind(act_tst,act_trn)
  rm(act_tst)
  rm(act_trn)
  values <- rbind(test_data,train_data)
  rm(test_data)
  rm(train_data)
  
  #converting the values into a single dataframe
  #Descriptive activity labels
  colnames(act_lbl) <- c("Activity_id","Activity")
  combined_data <- cbind(subjects,as.numeric(unlist(act)),values)
  rm(subjects)
  rm(act)
  names(combined_data) <- c("Subject","Activity_id","Values")
  combined_data <- combined_data %>% arrange(as.numeric(Subject),Activity_id) %>% full_join(act_lbl)
  
  #calculating average,standard deviation of values
  avg <- c()
  sdv <- c()
  for(i in 1:nrow(values)) {
    tmp <- values[[i,1]] %>% strsplit(split = " ") %>% unlist() %>% as.numeric()
    avg <- c(avg,mean(tmp,na.rm = T))
    sdv <- c(sdv,sd(tmp,na.rm = T))
  }
  combined_data <- cbind(combined_data,average = avg, SD = sdv) %>% select(!c(Values,Activity_id))
  
  #Tyding up the data 

  subject_details <- combined_data %>% select(c(Subject,Activity)) %>% unique() 
  experiment_details <- cbind(id = c(1:nrow(subject_details)),subject_details)
  
  experiment_values <- full_join(experiment_details,combined_data,by.x = c("Subject,Activity"),by.y = c("Subject,Activity")) %>%
    select(c(id,average)) %>% group_by(id) %>% summarise(average = mean(average)) 
  
  concise_data <- full_join(experiment_details,experiment_values) %>% View
  
}