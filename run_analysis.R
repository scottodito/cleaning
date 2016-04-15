######## worksheet.R #######################################################
# Script includes methods to download dataset, read and format, 
# finally summarizing the variables as means for each activity and subject
#
# Criteria
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation 
#     for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy 
#     data set with the average of each variable for each activity and 
#     each subject.
############################################################################

# @function readAndFormatTable
# @desiption : reads in and formats, merges, and returns a data set
# @return : formatted dataframe with variables of mean and standard deviation

readAndFormatTable <- function(){
  
  #download the files (if needed)
  downloadWorkingFiles()
  
  #read in dataframes
  x_test <- read.table("data/test/X_test.txt", header=FALSE)
  x_train <- read.table("data/train/X_train.txt", header=FALSE)
  
  #1) merge 2 tables
  merged_table_x <- rbind(x_test, x_train)
  
  #read in features data table rows for column names
  features_table <- read.table("data/features.txt", header = FALSE)

  colnames_as_features <- features_table$V2
    
  #clean the column name
  #convert camelcase to underscore
  colnames_as_features <- toUnderscore(colnames_as_features)
  #replace brackets with '_fn'
  colnames_as_features <- gsub("\\(\\)", "\\_fn", colnames_as_features)
  #remove ending bracket
  colnames_as_features <- gsub("\\)$", "", colnames_as_features)
  #replace other brackets with slashes
  colnames_as_features <- gsub("\\(|\\)|,|-", "_", colnames_as_features)
  #change std to standard-deviation
  colnames_as_features <- gsub( "std", "standard_deviation", colnames_as_features)
  
  colnames(merged_table_x) <- colnames_as_features
  
  #rename columns to features
  merged_table_x <- merged_table_x[,colnames_as_features]
  
  x_table_filtered <- merged_table_x[, grep("*mean*|*standard_deviation*", ignore.case = TRUE, colnames_as_features)]
  
  #get the test subject tables
  test_subjects_table <- read.table("data/test/subject_test.txt", header=FALSE)
  train_subjects_table <- read.table("data/train/subject_train.txt", header=FALSE)
  
  #merge tables together
  merged_subjects_table <- rbind(test_subjects_table, train_subjects_table)
  
  #rename column name to descriptive name
  merged_subjects_table <- rename(merged_subjects_table, subject_id = V1)
  
  #get the test activity ids
  test_activities_table <- read.table("data/test/y_test.txt", header = FALSE)
  train_activities_table <- read.table("data/train/y_train.txt", header = FALSE)
  
  #merge activity tables
  merged_activities_table <- rbind(test_activities_table, train_activities_table)
  
  #rename column name to descriptive name
  merged_activities_table <- rename(merged_activities_table, activity_id = V1)
  
  #finally join all 3 together
  joined_table_x <- cbind(x_table_filtered, merged_subjects_table, merged_activities_table)
  
  #get related table for label names  
  activity_labels <- read.table("data/activity_labels.txt", header=FALSE)
  
  #rename column name to descriptive name
  activity_labels <- rename(activity_labels, activity_id = V1, activity_name = V2)
  
  #get the final table
  final_table <- suppressWarnings(merge(joined_table_x, activity_labels))
  final_table <- final_table[,-final_table$activity_id]
  
  write.table(final_table, "large_rawish_table.txt")
  
  final_table
  
}

# run_analysis
# @desciption : saves tidy data set describing each subject with feature means for each activity
# @returns : tidy data set

run_analysis <- function(){
  
  #install dependencies
  
  if ("dplyr"  %in% rownames(installed.packages()) == FALSE) {
    install.packages("dplyr")
  }

  library(dplyr)

  #get the table after cleaning and joining
  final_table <- tbl_df(readAndFormatTable());
 
  #get the grouped data
  data_by <-  group_by(final_table, activity_name, subject_id)
  summary_output <- summarize_each(data_by, funs(mean))
  
  #export data
  write.table(summary_output, "tidy_data.txt", row.names = FALSE)
  
}

# @function downloadWorkingFiles
# @desciption dowloads files (if non existant)
downloadWorkingFiles <- function () {
  
  filedirectory <- "./data"
  
  #create data dir if needed
  if (!file.exists(filedirectory) ) {

    zip <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    datasetzip <- paste(filedirectory, "dataset.zip", sep = "/");
    datadir <- "UCI HAR Dataset"
  
    #download zip
    if (!file.exists(zip) ) {
      download.file(zip, datasetzip, method="curl")
    }
    
    #unzip files to data directoru
    unzip(datasetzip)
  
    #rename the dowloaded data dir to data
    file.rename(datadir, "data")
      
  }
  
}

# @function toUnderscore
# @description Converts the camel case to underscore case
toUnderscore <- function(x) {
  
  x2 <- gsub("([A-Za-z])([A-Z])([a-z])", "\\1_\\2\\3", x)
  x3 <- gsub(".", "_", x2, fixed = TRUE)
  x4 <- gsub("([a-z])([A-Z])", "\\1_\\2", x3)
  x5 <- tolower(x4)
  x5
  
}

#construct
run_analysis()