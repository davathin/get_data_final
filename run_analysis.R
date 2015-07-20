run_analysis<-function(){
  ##You should create one R script called run_analysis.R that does the following. 
  ##Merges the training and the test sets to create one data set.
  
  #Read in features for column header
  features<-read.table("features.txt")
  features[,2]<-as.character(features[,2])
  
  #Read in activity type
  activity_type<-read.table("activity_labels.txt")
  activity_type[,2]<-as.character(activity_type[,2])
  
  #Read in full test set
  subject_test<-read.table("test/subject_test.txt")
  x_test<-read.table("test/x_test.txt")
  y_test<-read.table("test/y_test.txt")
  
  #Read in full training set
  subject_train<-read.table("train/subject_train.txt")
  x_train<-read.table("train/X_train.txt")
  y_train<-read.table("train/y_train.txt")
  
  #Merge test and training sets
  x_merge<-rbind(x_train,x_test)
  
  #Name Columns
  colnames(x_merge) <- features$V2
  
  ##Extracts only the measurements on the mean and standard deviation for each measurement. 
  #Separate std and mean columns from merged set
  mean_merge<-grep("mean()", colnames(x_merge), fixed=TRUE)
  std_merge<-grep("std()", colnames(x_merge), fixed=TRUE)
  
  #Combine into single data set
  mean_std_merge<-x_merge[,c(mean_merge,std_merge)]
  
  ##Uses descriptive activity names to name the activities in the data set
  #Combine activity name data sets
  y_merge<-rbind(y_train,y_test)
  
  #Merge with std and mean data set
  mean_std_w_activity<-cbind(y_merge,mean_std_merge)
  
  #Rename activity column
  colnames(mean_std_w_activity)[1]<-"Activity_Type"
  
  #Change numberic key to human readable characters
  for (i in 1:nrow(mean_std_w_activity)){
    mean_std_w_activity[i,1]<-activity_type[mean_std_w_activity[i,1],2]
  }
  
  ##Appropriately labels the data set with descriptive variable names - Done above

  ##From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
  #Merge test and train subject ID sets
  subject_merge<-rbind(subject_train,subject_test)
  
  #Combine subject IDs with std and mean data set
  subject_semi<-cbind(subject_merge,mean_std_w_activity)
  
  #Rename Subject column
  colnames(subject_semi)[1] <- "Subject_ID"
  
  #Get mean for groups by subject ID and activity
  tidy_final <- aggregate(subject_semi,by=list(subject_semi$Subject_ID,subject_semi$Activity_Type),FUN="mean",data=subject_semi)
  
  #Rename new columns
  colnames(tidy_final)[1:2]<-c("Subject_ID","Activity_Type")
  
  #Get rid of bad columns
  tidy_final<-tidy_final[,-(3:4)]
  
  #Write final tidy tables
  write.table(tidy_final, file = "get_data_final.txt",row.name=FALSE)
}