getwd()
read.csv("UniversalBank.csv")
library(dplyr)
library(caret)
library(ISLR)
ml1<-select(UniversalBank,Age,Experience,Income,Family,CCAvg,Education,Mortgage,Personal.Loan,Securities.Account,CD.Account,Online,CreditCard)
ml1
summary(ml1)
set.seed(15)

## data partition
Train_Index1 <- createDataPartition(ml1$Age,p=0.5, list=FALSE) 
Train_Data1 = ml1[Train_Index1,]
Test_Index1 <- createDataPartition(ml1$Age,p=0.2, list=FALSE)
Test_Data1 = ml1[Test_Index1,]
Validate_Index1 <- createDataPartition(ml1$Age,p=0.3, list=FALSE)
Validate_Data1 = ml1[Validate_Index1,]

summary(Train_Data1)
summary(Test_Data1)
summary(Validation_Data1)

## normalization

Train.nf1<-Train_Data1
Validate.nf1<-Validate_Data1
Test.nf1<-Test_Data1

norm.values2<-preProcess(Train_Data1[,-8],method=c("center","scale"))
Train.nf1[,-8]<-predict(norm.values2,Train_Data1[,-8])
Test.nf1[,-8]<-predict(norm.values2,Test_Data1[-8])
Validate.nf1[,-8]<-predict(norm.values2,Validate_Data1[,-8])

summary(Train.nf1)
summary(Validate.nf1)

##creating levels for personal loan

Train_Data1$Personal.Loan<-factor(Train_Data1$Personal.Loan,levels = c(0,1),labels = c("deny","accept"))
Validate_Data1$Personal.Loan<-factor(Validate_Data1$Personal.Loan,levels = c(0,1),labels = c("deny","accept"))
Test_Data1$Personal.Loan<-factor(Test_Data1$Personal.Loan,levels = c(0,1),labels = c("deny","accept"))
Test_Data1$Personal.Loan
Validate_Data1$Personal.Loan
Train_Data1$Personal.Loan

## KNN modelling

#install.packages("FNN")#
# Training set #
library(FNN)
nnn1<-knn(train=Train.nf1[,-8],test=Train.nf1[,-8],cl=Train_Data1$Personal.Loan,k=4,prob=TRUE)
(nnn1)
library(gmodels)
CrossTable(x=Train_Data1$Personal.Loan,y=nnn1,prop.chisq = FALSE)
## we got the accuracy of 97.72% in the training data set ##


# Test set #
library(FNN)
nnn2<-knn(train=Train.nf1[,-8],test=Test.nf1[,-8],cl=Train_Data1$Personal.Loan,k=4,prob=TRUE)
(nnn2)
library(gmodels)
CrossTable(x=Test_Data1$Personal.Loan,y=nnn2,prop.chisq = FALSE)
## we got an accuracy of 97.40% in the test data set ##

# Validation set #
library(FNN)
nnn3<-knn(train=Train.nf1[,-8],test=Validate.nf1[,-8],cl=Train_Data1$Personal.Loan,k=4,prob=TRUE)
(nnn3)
library(gmodels)
CrossTable(x=Validate_Data1$Personal.Loan,y=nnn3,prop.chisq = FALSE)
## We got an accuracy of 97.66% ##

## From the Test data set to the Train data set we differ by an accuracy of 0.32% ##
## We got the 0.32% difference because we took the data partition of the test data as 20% and Train data as 50% so the test data has more data to analyse and give the more accuracy than the Test data set.
   

## From the Test data set to the Validation data set we differ by accuracy of 0.26% ##
## We got the 0.26% difference because we took the validation data as 30% and test data as 20% the validation partition has more data to predict and give more accuracy rate then the Test data set.
