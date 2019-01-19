#import data
data1 <- read.table("adult.data.txt",sep = ',')
colnames(data) <- c("Age","WorkClass","Fnlwgt","Education","EducationNum",
                    "MaritalStatus", "Occupation", "Relationship", "Race", "Sex", "CapitalGain",
                    "CapitalLoss","HoursPerWeek","NativeCountry","Class")

### original test data
data2 <- read.csv("adult.test.txt", sep = ",", header = F)
data2 <- data2[2:nrow(data2),]
#data2 <- data2[-1,]
colnames(data2) <- c("Age","WorkClass","Fnlwgt","Education","EducationNum",
                     "MaritalStatus", "Occupation", "Relationship", "Race", "Sex", "CapitalGain",
                     "CapitalLoss","HoursPerWeek","NativeCountry","Class")

#combine training and testing data
data <- rbind(data, data2)

#convert 50K_Indicator to binary data: >50K equals 1, <=50K equals 0
library(tidyverse)
data$Class <- str_replace(data$Class, ">50K", "1")
data$Class <- str_replace(data$Class, "<=50K", "0")
data$Class <- as.numeric(data$Class)
str(data)
write.csv(data,file="ProjectData.csv",row.names = FALSE)

#########################################################

# explore the data
head(data)
str(data)
sapply(data, table)
sapply(data, function(x) length(unique(x)))
sapply(data, function(x) sum(is.na(x)))

###############################################################








#############################################################
#PCA analysis
data <- read.csv("ProjectData.csv")
head(data)
data.pca <- prcomp(data[,c(1,3,5,11,12,13)], center= TRUE, scale.=TRUE)
library(ggplot2)
library(plyr)
library(scales)
library(grid)
library(ggbiplot)
ggbiplot(data.pca, groups = data$Class)

#PCA2
data$WorkClass <- as.numeric(data$WorkClass)
data$Education <- as.numeric(data$Education)
data$MaritalStatus <- as.numeric(data$MaritalStatus)
data$Occupation <- as.numeric(data$Occupation)
data$Relationship <- as.numeric(data$Relationship)
data$Race <- as.numeric(data$Race)
data$Sex <- as.numeric(data$Sex)
head(data)
data.pca2 <- prcomp(data[,1:13], center=TRUE, scale.=TRUE)
summary(data.pca2)
ggbiplot(data.pca2, groups=data$Class)

cor <- cor(data[,1:13])
corrplot::corrplot(cor)


#sampling the original dataset to 20000 instances
#set.seed(123)
#index <- sample(1:nrow(data),20000)
#data <- data[index,]

#####test git
