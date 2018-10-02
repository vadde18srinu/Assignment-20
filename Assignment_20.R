1. Use the below given data set
Data Set
2. Perform the below given activities:
a. Create classification model using different random forest models
b. Verify model goodness of fit
c. Apply all the model validation techniques
d. Make conclusions
e. Plot importance of variables

setwd("F:/AcadGild/workings")



lib=c("bigmemory", "readr", "Hmisc", "dplyr", "MASS", "ggplot2", "lattice", "caret", "rpart", 
      "randomForest", "rpart.plot","lattice", "rattle", "data.table","RColorBrewer", "reshape2",
      "InformationValue","stringr", "VIF", "Information", "Amelia", "gdata", "party","car", 
      "lubridate","zoo", "sqldf", "fuzzyjoin", "party", "mice")
sapply(lib, require, character.only=TRUE, quietly=TRUE)


dataset<-read.csv("F:/AcadGild/workings/Example_WearableComputing_weight_lifting_exercises_biceps_curl_variations_A20.csv")

# read data
dataset1<-fread("F:/AcadGild/workings/Example_WearableComputing_weight_lifting_exercises_biceps_curl_variations_A20.csv")

# list the levels for the class
sapply(dataset1, class)

d<-describe(dataset1)
d


# Delete all columns missing values
sapply(dataset1, function(x) sum(is.na(x))) # missing values  

Edata<-as.data.frame(dataset1)

Data <-Edata[,colSums(is.na(Edata)) == 0]
str(Data)
dim(Data)
View(Data)
sapply(Data,class)

# missing values verification

Amelia::missmap(Data) 

# Delete variables irrelevant to the current project
Training <- Data[,-c(1:6)]

# data partition is split in to 70%Training 30%Testing. 
set.seed(12345)
ind=sample(1:nrow(Training),0.70*nrow(Training),replace = FALSE)
Train<-Training[ind,]
Test<-Training[-ind,]

# exploratory analysis
dim(Train)
str(Train)
describe(Train)
head(Train)
summary(Train)

dim(Test)
str(Test)
describe(Test)
head(Test)


# Prediction First model - Decesion Tree
Model1 <- rpart(classe ~ ., data=Train, method="class")
summary(Model1)
#View the Decision Tree using fancy
fancyRpartPlot(Model1)

#Predicting
FirstPrediction <- predict(Model1, Test, type = "class")

# Using confusion Matrix to test results:
confusionMatrix(FirstPrediction, Test$classe)

#Second Prediction Model - Random Forests (redused variables 53 due to model not accepting above 53 variable)
Ttenth<-Train[,-c(5:8,31:36,55:63,40:41)]
dim(Ttenth)

Train1<-as.data.frame.table(Ttenth)


SecondModel <- randomForest(classe ~. , data=Ttenth, method="class")

#Predicting:
SecondPrediction <- predict(SecondModel, testtenth, type = "class")

#Test results on TestingTraining data set:
confusionMatrix(SecondPrediction, testtenth$classe)

#Testing the better model on original Testing Set
FinalPrediction <- predict(SecondModel, Ttenth, type="class")
FinalPrediction

confusionMatrix(FinalPrediction, Ttenth$classe)

describe(FinalPrediction)

summary(FinalPrediction)
df<-write.csv(FinalPrediction,"Assignment18pred.csv")
df


# goodnes of fit - chi square test. 

final.table<-table(FinalPrediction)
final.table

barplot(final.table, xlab="class", ylim = c(0,200))

final.proportion.tab<-(final.table/sum(final.table))
final.proportion.tab

barplot((final.proportion.tab)*100, main = "percentage of class", ylim = c(0,40))

chisq.test(final.table)

chisq.test(final.proportion.tab)


