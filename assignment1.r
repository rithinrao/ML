getwd()
x<-read.csv("Book1.csv")
x
class(x)

## descriptive stastics

mean(x[["Cols"]])
mean(x[,"Cols"])
mean(x[["Cols"]],trim=20,na.rm=TRUE)
median(x[["Cols"]],na.rm=TRUE)

mean(x$Cols)
min(x$Cols)
max(x$Cols)
max(x[,5])
which.max(x$Cols)
x[31,]
x[which.min(x$cols),]

library(moments)
skewness(x$Cols)

range(x$Cols)
var(x$Cols)
sd(x$Cols)
quantile(x$Cols)
IQR(x$Cols)

## scatterplots

p<-x$Rows
q<-x$Cols
plot(q,p,main="scatterplot",xlab="number of rows",ylab="number of cols",frame=FALSE)
abline(lm(q~p,data=x),col="blue")

## histogram

hist(x$Cols,col="yellow")
hist(x$Rows,col="blue")

##boxplot

boxplot(Cols~Rows,data=x,main="boxplot")

##Transformation of quantative variables

lg<-log10(x$Cols)
sq<-sqrt(x$Cols)
rt<-sqrt(x$Rows)
rt

install.packages("dplyr")
library(dplyr)
arrange(x,x$Cols)
arrange(x,x$Rows)
