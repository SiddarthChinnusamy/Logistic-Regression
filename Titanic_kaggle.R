library("ggplot2")
library("dplyr")
library("ROCR")
library("WVPlots")
library("foreign")
library("vtreat")
library("pROC")
library("Amelia")
setwd("D:/Kaggle")

# Importing training and test datasets
train<-read.csv("D:/Kaggle/train.csv",stringsAsFactors = F)
test<-read.csv("D:/Kaggle/test.csv",stringsAsFactors = F)

# Exploring the training dataset
summary(train)
any(is.na(train))
sum(is.na(train))
str(train)
missmap(train)

#Replacing missing values in Age column with median age
train$Age[which(is.na(train$Age))]<-median(train$Age,na.rm=TRUE)
sum(is.na(train))
missmap(train)

#Converting appropriate variables to categorical variables 
train$Survived<-as.factor(train$Survived)
train$Pclass<-as.factor(train$Pclass)
train$Sex<-as.factor(train$Sex)
train$SibSp<-as.factor(train$SibSp)
train$Parch<-as.factor(train$Parch)
train$Embarked<-as.factor(train$Embarked)
str(train)

# Reflecting the changes in test data set
test$Pclass<-as.factor(test$Pclass)
test$Sex<-as.factor(test$Sex)
test$SibSp<-as.factor(test$SibSp)
test$Parch<-as.factor(test$Parch)
test$Embarked<-as.factor(test$Embarked)
str(test) 

# Baseline model accuracy
table(train$Survived)
BA <- (549/(549+342))
BA

#Feature Engineering
# #Exploratroy analysis by plotting and Hypothesis testing
ggplot(train,aes(x=Age))+geom_histogram(binwidth=10)

#Pclass and Survival
ggplot(train,aes(Pclass,Survived,color=Survived)) + geom_jitter()+ggtitle("Passenger Class Vs. Survival")
ggplot(train,aes(Pclass,..count..))+geom_bar(aes(fill=Survived),position="fill")+ggtitle("Passenger Class Vs. Survival")
chisq.test(train$Pclass, train$Survived)#PClass affects survival rate


# Age and Survival
ggplot(train,aes(x=Age,y=Survived,color=Survived))+geom_point()
ggplot(train,aes(Age,Survived,color=Survived))+geom_jitter()
t.test(train$Age~train$Survived)#Age affects survival rate marginally


# Gender and Survival
ggplot(train,aes(Sex,Survived,color=Survived))+geom_jitter()
chisq.test(train$Sex, train$Survived)#Gender affects survival rate


# Presence of siblings and Survival
ggplot(train,aes(SibSp,Survived,color=Survived)) + geom_jitter()
ggplot(train,aes(SibSp,..count..))+geom_bar(aes(fill=Survived),position="fill")+ggtitle("SiblingSp Vs. Survival")
chisq.test(train$SibSp, train$Survived)#Not reliable result


# Presence of Parent/child for survival
ggplot(train,aes(Parch,factor(Survived))) + geom_jitter()
ggplot(train,aes(Parch,..count..))+geom_bar(aes(fill=Survived),position="fill")+ggtitle("Parch Vs. Survival")

ggplot(train,aes(Cabin,factor(Survived))) + geom_jitter()
chisq.test(train$Parch, train$Survived)#not reliable result

# Fare and survival
ggplot(train,aes(Fare,factor(Survived))) + geom_jitter()+facet_grid(.~Sex)
t.test(train$Fare~train$Survived)#Fare affects survival rate


#port of embarkment
ggplot(train,aes(Survived,Embarked))+geom_jitter()
chisq.test(train$Embarked, train$Survived)#not reliable result
ggplot(train,aes(Embarked,..count..))+geom_bar(aes(fill=Survived),position="fill")+ggtitle("Parch Vs. Survival")


#Plot with 3 Variables
ggplot(train,aes(Sex,Survived,color=Survived))+geom_jitter()+facet_grid(.~Pclass)
ggplot(train,aes(Sex,Survived,color=Survived))+geom_jitter()+facet_grid(.~Embarked)


#Based on the plots and hypothesis tested the 
#following variables are included in the model

# Formula for the model
formula<-Survived~Pclass+Sex+Age+Fare


# Training by Cross validation
n<-nrow(train)
splitPlan<-kWayCrossValidation(n,3,NULL,NULL)
str(splitPlan)
k<-3
train$pred.cv<-0
for (i in 1:k){
  split<-splitPlan[[i]]
  model<-glm(formula,data=train[split$train,],family=binomial)
  train$pred.cv[split$app]<-predict(model,newdata=train[split$app,],type="response")
}
summary(model)
# Predicting for full training data
train$predictions<-predict(model,newdata=train,type="response")

# Gain curve plot of predictions vs actual data for training data
GainCurvePlot(train,"pred.cv","Survived","GCplot_Cross Validation")
#GainCurvePlot(train1,"predictions","Survived","GCplot")



# Confusion matrix
cm<-(table(Actualvalue = train$Survived, PredictedValue = train$pred.cv>0.5))
cm
#Derived metric from confusion matrix
#accuracy
accuracy <- (cm[1,1]+cm[2,2])/(cm[2,2] + cm[1,2] + cm[1,1] + cm[2,1]); accuracy

# precision
precision <- cm[1,1] / (cm[1,1]+cm[1,2]); precision

# Sensitivity
sensitivity<- cm[2,2]/(cm[2,2] + cm[1,2]); sensitivity

#Specifity
specificity<- cm[1,1]/(cm[1,1] + cm[2,1]); specificity

#recall
recall <- cm[1,1] / (cm[1,1]+cm[2,1]); recall



# ROC Curve
pred2 <- prediction(train$pred.cv, train$Survived)
roc.perf <- performance(pred2, measure = "tpr", x.measure = "fpr")
plot(roc.perf,colorize=TRUE,print.cutoffs.at=seq(0.1,by=0.1))
roc(train$Survived,train$pred.cv,plot=TRUE)


#prediction on test data
test$predictions<-predict(model,newdata=test,type="response")
str(test)
write.csv(test,file="Results.csv")
