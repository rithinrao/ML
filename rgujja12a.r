---
title: "R Notebook"
output: html_notebook
---

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook. When you execute code within the notebook, the results appear beneath the code. 

Try executing this chunk by clicking the *Run* button within the chunk or by placing your cursor inside it and pressing *Ctrl+Shift+Enter*. 

```{r}
getwd()
read.csv("UniversalBank.csv")
library(dplyr)
library(caret)
library(ISLR)
ml1<-select(UniversalBank,Age,Experience,Income,Family,CCAvg,Education,Mortgage,Personal.Loan,Securities.Account,CD.Account,Online,CreditCard)
set.seed(15)

## data partition

Train_Index=createDataPartition(ml1$Age,p=0.6,list=FALSE)
Train_Data=ml1[Train_Index,]
Train_Data
Validation_Data=ml1[-Train_Index,]

summary(Train_Data)

## normalization

Train.nf<-Train_Data
Validation.nf<-Validation_Data

norm.values<-preProcess(Train_Data[,-8],method=c("center","scale"))
Train.nf[,-8]<-predict(norm.values,Train_Data[,-8])

norm.values1<-preProcess(Validation_Data[,-8],method=c("center","scale"))
Validation.nf[,-8]<-predict(norm.values1,Validation_Data[,-8])

summary(Train.nf)
summary(Validation.nf)

## creating factors ##

Train_Data$Personal.Loan<-factor(Train_Data$Personal.Loan,levels = c(0,1),labels = c("deny","accept"))
Validation_Data$Personal.Loan<-factor(Validation_Data$Personal.Loan,levels = c(0,1),labels = c("deny","accept"))
Train_Data$Personal.Loan

## KNN modelling

#install.packages("FNN")#
library(FNN)
nn<-knn(train=Train.nf[,-8],test=Validation.nf[,-8],cl=Train_Data$Personal.Loan,k=1,prob=TRUE)
(nn)
levels(nn)

#install.packages("gmodels")#
library(gmodels)
CrossTable(x=Validation_Data$Personal.Loan,y=nn,prop.chisq = FALSE)

## finding the best k ##

library(caret)
Bestk <- data.frame(k = seq(1, 55, 1), accuracy = rep(0, 55))

for(i in 1:55) {
  knn.pred <- knn(Train.nf[,-8],Validation.nf[,-8],
                  cl =Train_Data$Personal.Loan, k = i)
  Bestk[i,2]<-confusionMatrix(knn.pred,Validation_Data$`Personal.Loan`)$overall[1]
}
Bestk

Bestk[which.max(Bestk$accuracy),]

## confusion matrix for best k=4##

library(FNN)
nn<-knn(train=Train.nf[,-8],test=Validation.nf[,-8],cl=Train_Data$Personal.Loan,k=4,prob=TRUE)
library(gmodels)
CrossTable(x=Validation_Data$Personal.Loan,y=nn,prop.chisq = FALSE)

## predicting for the customer ##

c1<-data.frame("Age"=40,"Experience"=10,"Income"=84,"Family"=2,"CCAvg"=2,"Education"=1,"Mortgage"=0,"Personal.Loan"=0,"Securities.Account"=0,"CD.Account"=0,"Online"=1,"CreditCard"=1)
c2<-data.frame("Age"=40,"Experience"=10,"Income"=84,"Family"=2,"CCAvg"=2,"Education"=2,"Mortgage"=0,"Personal.Loan"=1,"Securities.Account"=0,"CD.Account"=0,"Online"=1,"CreditCard"=1)
c3<-data.frame("Age"=40,"Experience"=10,"Income"=84,"Family"=2,"CCAvg"=2,"Education"=3,"Mortgage"=0,"Personal.Loan"=0,"Securities.Account"=0,"CD.Account"=0,"Online"=1,"CreditCard"=1)
cc<-as.data.frame(rbind(c1,c2,c3))
class(cc)

cc.norm<-ll[,-8]
nor<-preProcess(cc.norm,method=c("center","scale"))
ll.norm<-predict(nor,cc.norm)
pp<-knn(Train.nf[,-8],ll.norm,cl=Train_Data$Personal.Loan,k=4,prob = TRUE)
pp

```

#Add a new chunk by clicking the *Insert Chunk* button on the toolbar or by pressing *Ctrl+Alt+I*.

#When you save the notebook, an HTML file containing the code and output will be saved alongside it (click the *Preview* button or press *Ctrl+Shift+K* to preview the HTML file).

#The preview shows you a rendered HTML copy of the contents of the editor. Consequently, unlike *Knit*, *Preview* does not run any R code chunks. Instead, the output of the chunk when it was last run in the editor is displayed.
