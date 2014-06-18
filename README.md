


# Getting and Cleaning Data Course Project 

README.md

a code book that describes describing how the script (run_analysis.R) works, the variables, the data, and any work to clean up the data 



## Run the script
To run the script just type the following command in R

```r
source("run_analysis.R")

```


## How the script (run_analysis.R) works
1.  Download the data.
2.  Extracts only the columns that calulate the mean and standard deviation for
each measurement using the function mean() and std().  There are altogether 66
such comlumns. A helper function called processtring_2() is created for this
purpose.
3.  Merges the training and the test sets to create one data set, which is
called x_mean_std.
4. Appropriately labels the data set with descriptive variable names. For
example, variable "tBodyAcc-mean()-X" is named by label "tBodyAcc_mean_X".
5. Uses descriptive activity names to name the activities in the data set. For
example, use "WALKING" to name the acticity with label=1.
6. Add subject data. There are altogether 30 subjects (volunteers). 
7. Create data frame all_data_df to  include all infomation 
8. Creates a second, independent tidy data set with the average of each variable
for each activity and each subject.  This data frame is named by tidy_data
9. Save tidy_data to a csv file named "tidy_data.csv"




