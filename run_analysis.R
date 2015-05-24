# run_analysis.R  getting and cleaning data course project module
#                 This function takes 3 parameters; 
#                                1. the location of the data specified for the course project
#                                2. The full path name of the csvb file to store the average data defined in requirement 5
#
#      DATE            by                         Comments
# 22MAY2015        markcovello@comcast.net       initial build
#
# flow:
# for each study_group
# 1. read measures 
# 2  rename measures columns from features.txt
# 3  subset columns required for project (std()/mean())
# 4. read subject list for measures data
# 5. read activities list for measures data 
# 6. convert activities list to string equivalents
# 7. merge subject, activities, measures for test/train; identify Train/Test data 
# Then assemble the remaining steps
# 8. append test to train
# 9. calculate averages for merged data by activity/subject
# 10. rename average variables
# 11. write out results
# Note that this assumes the zip file has been expanded;
# the project requirements do not include reading the data at all, 
# much less reading the data from a zip archive; but using the single parent directory for the data
# as the only parameter seemed a convenient way to structure the approach
# data_root is the directory into which the zipped file has been expanded
# Please see the codebook(https://github.com/markcovello-home/getting_cleaning_data/blob/master/Project_Codebook.pdf) for a more detailed explanation of the solution.


run_analysis <- function(data_root,avecsv,pdscsv) {
  
  # 1a. read measures
  fileloc <- paste(data_root,"/UCI HAR Dataset/test/x_test.txt",sep="")
  test_measures <- read.table(fileloc)
  # 2a.rename measures columns from features.txt
  fileloc <- paste(data_root,"/UCI HAR Dataset/features.txt",sep="")
  measure_names <- read.table(fileloc)
  names(test_measures) <- measure_names$V2
  # 3a.  subset columns required for project (std()/mean())  
  pat <- "mean\\(\\)|std\\(\\)"
  test_measures <- subset(test_measures, , grep(pat,names(test_measures)))
  
  
  # from development work this should yield 66 columns
  
  # 4a. read subject list for measures data
  fileloc <- paste0(data_root,"/UCI HAR Dataset/test/subject_test.txt")
  test_subjects <- read.table(fileloc)
  
  # 5a. read activities list for measures data
  fileloc <- paste0(data_root,"/UCI HAR Dataset/test/y_test.txt")
  test_activities <- read.table(fileloc)
  
  # 6a. convert activities list to string equivalents
  fileloc <- paste0(data_root,"/UCI HAR Dataset/activity_labels.txt")
  activities <- read.table(fileloc)
  luactnames <- c("token","activity")
  names(activities) <-  luactnames 
  names(test_activities)<- "token"
  test_activities <- merge(test_activities,activities,by="token", all=TRUE)
  test_activities <- subset(test_activities, , "activity")
  
  # 7a. merge subject, activities, measures for test/train
  names(test_subjects)<- "subject"
  
  #so we have all the items for the Human Activity Project Data Store
  # except for  the study group identifier(test/train)
  # add that to the subjects frames
  
  # 7a. merge subject, activities, measures for test/train identify Train/Test data 
  library(plyr)
  test_subjects <- mutate(test_subjects,study_group="test")
  testleft <- cbind(test_subjects,test_activities)
  
  testdata <- cbind(testleft,test_measures)
  # that completes the test records 
  
  # repeat for train
  # 1b. read measures
  fileloc <- paste(data_root,"/UCI HAR Dataset/train/x_train.txt",sep="")
  train_measures <- read.table(fileloc)
  
  # 2b.rename measures columns from features.txt
  names(train_measures) <- measure_names$V2
 
  # 3b.  subset columns required for project (std()/mean())  
  train_measures <- subset(train_measures, , grep(pat,names(train_measures)))
  
  # 4b. read subject list for measures data
  fileloc <- paste0(data_root,"/UCI HAR Dataset/train/subject_train.txt")
  train_subjects <- read.table(fileloc)
  
  # 5b. read activities list for measures data
  fileloc <- paste0(data_root,"/UCI HAR Dataset/train/y_train.txt")
  train_activities <- read.table(fileloc)
  
  # 6b. convert activities list to string equivalents
  names(train_activities)<- "token"
  train_activities <- merge(train_activities,activities,by="token", all=TRUE)
  train_activities <- subset(train_activities, , "activity")
  
  # 7b. merge subject, activities, measures for test/train identify Train/Test data 
  names(train_subjects)<- "subject"
  train_subjects <- mutate(train_subjects,study_group="train")
  trainleft <- cbind(train_subjects,train_activities)
  traindata <- cbind(trainleft,train_measures)
  
  # Then assemble the remaining steps
  # 8. append test to train
  project_data_store <- rbind(traindata,testdata)  
  
  # 9. calculate averages for merged data by activity/subject
  library(dplyr)
  avetestdata <- project_data_store %>% group_by(subject, study_group, activity) %>% summarise_each(funs(mean))  
  # 10. rename average variables
  tnames <-names(avetestdata)
  appendtext <- "AVE"
  newnames <- paste0(appendtext,tnames)
  newnames[1:3] <- tnames[1:3]
  names(avetestdata) <- newnames
  # 11. write out results
  write.csv(avetestdata,avecsv,row.names=FALSE)
  write.csv(project_data_store,pdscsv,row.names=FALSE)
  
  
