setwd("coursera//data-cleaning//assignment//datacleaningassignment")
####################################################################
#load packages
install.packages("dplyr")
install.packages("stringr")
install.packages("sqldf")
library(dplyr)
library(stringr)
library(sqldf)
library(data.table)


####################################################################
#load activity codes
activitycodes <- read.table(".//data//activity_labels.txt", header = FALSE)
names(activitycodes) <- c("activitycode","activity")

####################################################################
#load features
features <- read.table(".//data//features.txt", header = FALSE, stringsAsFactors = FALSE)
names(features) <- c("row", "label")

#get duplicates
indx <- duplicated(features$label) | duplicated(features$label, fromLast = TRUE)

ord <- arrange(features[indx, ], label) #arrange by label
ord <- ord %>%
       mutate(x = rep(c("-X", "-Y", "-Z"), length = 126),
              label = paste(label, x, sep=""))
ord <- arrange(ord, row) #arrange by row

features[indx, "label"] <- ord$label 
####################################################################
#load train and test data
train_set <- read.table(".//data//train//X_train.txt")
train_label <- read.table(".//data//train//Y_train.txt")
test_set <- read.table(".//data//test//X_test.txt")
test_label <- read.table(".//data//test//Y_test.txt")

#combine set and label data
train_set <- cbind(train_set, train_label)
test_set <- cbind(test_set, test_label)
head(train_set)


#task 1: Merges the training and the test sets to create one data set.
combined <- rbind(train_set, test_set) #combine test and train datasets

#task 4: Appropriately labels the data set with descriptive variable names.
names(combined) <- c(features$label, "activitycode") #assign column name from features

#task 3: Uses descriptive activity names to name the activities in the data set
#for joining of activity table:
combined <- mutate(combined,
                    rownum = rownames(combined)) 

#join to activity
res <- sqldf("select t.*, a.activity 
          from combined t
          inner join activitycodes a
          on t.activitycode = a.activitycode")

#get list of columns with mean and std
n <- names(res)
n <- n[(n %like% 'mean' | n %like% 'std' | n == "activity")]

#task 2: Extracts only the measurements on the mean and standard deviation for each measurement.
tidydata1 <- res[,n]
head(tidydata1)

#task 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
tidydata2 <- aggregate(tidydata1[1:79], 
          by=list(tidydata1$activity),
          mean)


#export tidy data
write.table(tidydata1, "tidydata1.txt", row.names = FALSE)
write.table(tidydata2, "tidydata2.txt", row.names = FALSE)
