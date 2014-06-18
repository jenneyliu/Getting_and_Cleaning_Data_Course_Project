

rm(list=ls())
ls()


# Downloading and unziping the data
dataUrl  <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
file  <- "oridata.zip"
download.file(url=dataUrl, destfile=file, method="curl")
# Methods "wget" and "lynx" are mainly for historical compatibility, but they and "curl" can be used for URLs (e.g. https:// URLs or those that use cookies) which the internal method does not support. They will block all other activity on the R process.
unzip(zipfile=file, exdir="./")
filename = gsub(pattern=".zip", replacement=".txt", x=file)
filename


train_dir <- "./UCI HAR Dataset/train"
test_dir <- "./UCI HAR Dataset/test"

train_x_file <- "X_train.txt"
train_y_file <- "y_train.txt"
train_subject_file <- "subject_train.txt"
test_x_file <- "X_test.txt"
test_y_file <- "y_test.txt"
test_subject_file <- "subject_test.txt"


features_file <- "./UCI HAR Dataset/features.txt"
activity_label_file <- "./UCI HAR Dataset/activity_labels.txt"



# Loadind the dataset in a data frame
train_x  <- read.table(file=paste(train_dir, train_x_file, sep = "/"), stringsAsFactors=F, header=F, sep = "")
train_y  <- read.table(file=paste(train_dir, train_y_file, sep = "/"), stringsAsFactors=F, header=F, sep = "")
train_subject <- read.table(file=paste(train_dir, train_subject_file, sep = "/"), stringsAsFactors=F, header=F, sep = "")


test_x  <- read.table(file=paste(test_dir, test_x_file, sep = "/"), stringsAsFactors=F, header=F, sep = "")
test_y  <- read.table(file=paste(test_dir, test_y_file, sep = "/"), stringsAsFactors=F, header=F, sep = "")
test_subject <- read.table(file=paste(test_dir, test_subject_file, sep = "/"), stringsAsFactors=F, header=F, sep = "")


colnames <- readLines(features_file)
activity_label <- readLines(activity_label_file)





dim(train_x)
dim(train_y)
dim(train_subject)
dim(test_x)
dim(test_y)
dim(test_subject)







# define function to help to extract only the measurements on the mean and standard deviation for each measurement. 
processtring_2 <- function(variable_str)
{
      #a <- gsub("\\()", "", variable_str)
      a <- variable_str
      b <- strsplit(a, " ")[[1]]
      
      c <- b[2]
      if(grepl("-",b[2]))  { c <- c(strsplit(b[2], "-")[[1]]) }
      
      c1 <- character()
      if(sum(grepl("\\.", c)))   { for(i in 1:length(c)) {c1 <- c(c1, c(strsplit(c[i], "\\.")[[1]])) } }
      
      
      c2 <- c(c, c1)
      d <- paste(c2, collapse ="_")
      
      
      mean_str <- "-mean\\()"
      mean <- character()
      if(grepl(mean_str, b[2], ignore.case = T))
      {
            start <- as.integer( gregexpr(mean_str, b[2], ignore.case=T)[[1]])
            end <- start + nchar(mean_str) -1 
            
            if(length(start)>=2)   { 
                  message("error: more than 2 substring")
                  print(b[2])
                  print(mean_str)
            }
            
            mean <- substr(b[2], start[1], end[1])
      }
      
      
      
      std_str <- "-std\\()"
      std <- character()
      if(grepl(std_str, b[2], ignore.case = T))
      {
            
            start <- as.integer( gregexpr(std_str, b[2], ignore.case=T)[[1]])
            end <- start + nchar(std_str) -1 
            
            if(length(start)>=2)   { 
                  message("error: more than 2 substring")
                  print(b[2])
                  print(std_str)
            }
            
            std <- substr(b[2], start[1], end[1])
      }
      
      
      
      
      # mean_std_flag <- c2 %in% c("mean", "std")
      # mean_std <- c2[mean_std_flag]
      
      mean_std <- c(mean, std)
      
      if(length(mean_std) ==0 ){mean_std <- NA}
      if(length(mean_std) !=1 ){print(mean_std)
                                message("error: both mean and std exist or length error")
      }
      
      d <- gsub("\\()", "", d)
      
      
      variable_labels <- c(b[1], d, mean_std)
}







# Extracts only the measurements on the mean and standard deviation for each measurement. 
# library(stringr)
new_col_label <- sapply(colnames, processtring_2)
new_col_label <- t(new_col_label)
new_col_label_df <- data.frame(new_col_label)
colnames(new_col_label) <- c("col_num", "labels", "mean_std")

mean_std_colnames <- data.frame( new_col_label[which(!is.na(new_col_label[, 3])), ] )
mean_std_colnames$colind <- as.integer(as.character(mean_std_colnames[, 1]))









# Merges the training and the test sets to create one data set.
x <- rbind(train_x, test_x)
x_df <- data.frame(x)
dim(x_df)
x_mean_std <- x_df[, mean_std_colnames$colind]


# Appropriately labels the data set with descriptive variable names. 
colnames(x_mean_std) <- mean_std_colnames$labels






# Uses descriptive activity names to name the activities in the data set
activity_label_2 <- sapply(activity_label, function(x) strsplit(x, " ")[[1]])
activity_label_2 <- t(activity_label_2)
activity_label_df <- data.frame(activity_label_2)
colnames(activity_label_df) <- c("id", "activity_label")
activity_label_df$id <- as.integer(activity_label_df$id )


y <- rbind(train_y, test_y)
y_df <- data.frame(y)


colnames(y_df) <- "activity_label"
y_df$activity_label <- as.integer( as.character(y_df$activity_label) )
y_df$activity_label  <- activity_label_df$activity_label[y_df$activity_label]




# add subject data
subject_data <- rbind(train_subject, test_subject)
subject_data_df <- data.frame(subject_data)
colnames(subject_data_df) <- "subject"



# Create data frame all_data_df to  include all infomation
all_data_df <- data.frame(x_mean_std, subject=subject_data_df$subject, activity_label= y_df$activity_label)







# Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
ind <- list(subject=as.factor(all_data_df$subject), activity=as.factor(all_data_df$activity_label))
splidata <- split(all_data_df, ind)
tidy_data  <- sapply(splidata, function(x) apply(x[, 1:66], 2, mean))
colnames(tidy_data) <- paste("Subject", colnames(tidy_data), sep="")
tidy_data <- data.frame(variable=rownames(tidy_data), tidy_data)
rownames(tidy_data) <- NULL


# Save tidy_data to a csv file named “tidy_data.csv”
# write.csv(file="./UCI HAR Dataset/tidy.csv", x=tidy_data, col.names = TRUE, row.names=T)
write.table( x=tidy_data, file="./tidy_data.csv", sep = ",", col.names = TRUE, row.names=F)





