url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

##get the path of the working directory
file_path <- getwd()
if (!file.exists(file_path)) {
  dir.create(file_path)
}

## required package "data.table"
## require function work as ibrary() , also return logical

if (!require("data.table")) {
  install.packages("data.table")  ##install the package if doesn't exist
  require(data.table)
}

## required package "dplyr"
if (!require("dplyr")) {
  install.packages("dplyr") ##install the package if doesn't exist
  require("dplyr")
}

## required package "tidyr"
if (!require("tidyr")) {
  install.packages("tidyr")  ##install the package if doesn't exist
  require("tidyr")
}

## required package "purrr"
if (!require("purrr")) {
  install.packages("purrr")  ##install the package if doesn't exist
  require("purrr")
}

##download the datafile in the working directory, with the name data.zip
download.file(url, file.path(file_path, "data.zip"))
new_path <- file.path(file_path, "UCI HAR Dataset")

##read subject data
subject_train <- fread(file.path(new_path,"train","subject_train.txt"))
subject_test <- fread(file.path(new_path,"test","subject_test.txt"))

##read y_train, Y_test, X_train and X_test respectively
Y_train <- fread(file.path(new_path, "train", "Y_train.txt"))
Y_test <- fread(file.path(new_path, "test", "Y_test.txt"))
X_train <- read.table(file.path(new_path, "train", "X_train.txt"))
X_test <- read.table(file.path(new_path, "test", "X_test.txt"))

##bind subject data by rows
subject <- rbind(subject_train, subject_test) %>%
  rename(subject=V1)

##bind Y_train and Y_test by rows
Y <- rbind(Y_train, Y_test)%>%
  rename(Y=V1)

##bind X_train and X_test by rows
X <- rbind(X_train, X_test)

## bind all data by cols .. 
merged_data <- cbind(subject, Y, X)

##read features data
features <- fread(file.path(new_path, "features.txt")) %>%
  rename(id=V1, func=V2)

## get specific indecies of features add a new variable var_names
features <- features[grepl("mean\\(\\)|std\\(\\)",features$func),] %>%
  mutate(var_names = paste0("V",id))

##select specific cols by using select 
select <- c("subject", "Y", features$var_names)
merged_data <- merged_data[,select,with=F]

##read activity labels
activity_labels <- fread(file.path(new_path, "activity_labels.txt")) %>%
  rename(num=V1, activity_name=V2)

##merge activite_labels with current data 
## gather all varibles that starts with letter "V" 
gathered_data <- merge(merged_data, activity_labels, by.x = "Y", by.y = "num" ) %>%
  mutate(row=1:nrow(merged_data)) %>%
  gather(key = key, value = value, starts_with("V",ignore.case = T))

## spread the gathered data to the original shape
tidy_data <- gathered_data %>%
  group_by(activity_name, subject)%>% ## group data by activity_name and subject 
  spread( key, value)%>%
  map_at(.at =  starts_with("^V+[1-9]+",ignore.case=T) ,mean) ## walk through data and calculate the average by mean function 

write.table(tidy_data, file = "./tidy_data.txt")








