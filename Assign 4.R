run_analysis.R <- function(Run) {
  
library(dplyr)
library(plyr)
# Read data source

Ziplocation <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipfile <- "UCIDataset.zip"

if(!file.exists(zipfile)){
  
download.file(Ziplocation,zipfile, mode = "wb")
}

datafile <- "UCI_Dataset"
if(file.exists(zipfile))
  unzip(zipfile)

UCIHARDataset <- "UCI HAR Dataset"
#Read the data file
SubjectTest <-read.table(file.path(UCIHARDataset,"test","subject_test.txt"))
XTest <- read.table(file.path(UCIHARDataset,"test","x_test.txt"))
YTest <- read.table(file.path(UCIHARDataset,"test","y_test.txt"))

SubjectTrain <- read.table(file.path(UCIHARDataset,"train","subject_train.txt"))
XTrain <- read.table(file.path(UCIHARDataset,"train","X_train.txt"))
YTrain <- read.table(file.path(UCIHARDataset,"train","y_train.txt"))

Features <- read.table(file.path(UCIHARDataset,"features.txt"))
activity <- read.table(file.path(UCIHARDataset,"activity_labels.txt"))


#Merge data

TrainingData <- cbind (SubjectTrain,XTrain,YTrain)
TestData <- cbind(SubjectTest, XTest, YTest)

Allraw <- rbind(TrainingData,TestData)

rm(Features,SubjectTrain,SubjectTest,TestData,TrainingData,XTest,YTrain,activity,XTrain,YTest)

# name columns


colnames(Allraw) <- c("subject", Features[,2],"activity")


Columntoparse <- grepl("subject|activity|mean|std",colnames(Allraw))

SubsetedDataset <- Allraw[, Columntoparse]

SubsetedDataset$activity <- factor(SubsetedDataset$activity, 
                                 levels = activity[, 1], labels = activity[, 2])


Names_clean <- colnames(SubsetedDataset)

Names_clean <- gsub("[\\(\\)-)]","", Names_clean)

Names_clean <- gsub("^t","Timeclean",Names_clean)
Names_clean <- gsub("acc","Accelemetor", Names_clean)
Names_clean <- gsub("std","Standard Deviation", Names_clean)
Names_clean <- gsub("Gyro", "Gyroscope",Names_clean)
Names_clean <- gsub("Freq", "Frequency", Names_clean)
Names_clean <- gsub("BodyBody","Body",Names_clean)

colnames(SubsetedDataset) <- Names_clean

Completedata <- SubsetedDataset %>%
  group_by(subject, activity ) %>%
  summarise_each(funs(mean)) 
  
 write.table(Completedata,"Tidydata.txt", row.names = FALSE)

}


 
  
  
  
