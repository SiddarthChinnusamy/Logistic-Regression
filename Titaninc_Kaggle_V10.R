library(data.table)
library(plyr)
library(stringr)
library(Amelia)
setwd("D:/Titanic")
training<-read.csv("train.csv",na.strings=c(""," ",NA,"NA"),header=T)
training
#Features
#class - Y/N
#Name - No
#Sex - Y - F(P)
#SibSp - Y
#Parch - Parents/Children-Y
#ticket - N
#Fare - N
#Cabin - Y
#Embarkment

#Exploratory analysis
#Viewing data
head(training)
summary(training)
str(training)

#Changing data types appropriately
training$Survived<-as.factor(training$Survived)
training$Pclass<-as.factor(training$Pclass)

##Handling missing values
## 687 NAs in Cabin
##  2  NAs in Embarked
sapply(training,function(x)sum(is.na(x)))
sapply(training,function(x)length(unique(x)))
missmap(training,main="Missing values vs observed")

data<-subset(training,select=c(2,3,5,6,7,8,10,12))
str(data)
## 177 NAs in Age
hist(data$Age)
data$Age[is.na(data$Age)]<-median(data$Age,na.rm=T)
data$Age
data<-data[!is.na(data$Embarked),]
str(data)
#Splitting test and train data
trainf<-data[1:800,]
testf<-data[801:889,]
trainf

str(f)
table(trainf$Survived)

# fitting a model
model<-glm(Survived~.,family=binomial(link="logit"),data=trainf)
summary(model)
anova(model,test="Chisq")

model1<-glm(Survived~Pclass+Sex+Age+SibSp,family=binomial(link="logit"),data=trainf)
summary(model1)
anova(model1,test="Chisq")

#predictions on test data
pred1<-predict(model,newdata=testf,type="response")
pred1<-ifelse(pred1>0.4,1,0)

#Predictions on train data
pred2<-predict(model,newdata=trainf,type="response")
plot(pred2,trainf$Survived)

##Confusion matrix
conf_matrix<-table(pred1,testf$Survived)
conf_matrix









