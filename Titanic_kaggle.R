library("ggplot2")
library("dplyr")
library("ROCR")
library("WVPlots")
library("foreign")
library("vtreat")
setwd("D:/Kaggle")

# Importing training and test datasets
train<-read.csv("D:/Kaggle/train.csv",stringsAsFactors = F)
test<-read.csv("D:/Kaggle/test.csv",stringsAsFactors = F)

# Exploring the training dataset
summary(train)
any(is.na(train))
sum(is.na(train))
str(train)
summary(train)

#Removing rows with NA values (177 rows removed)
train1<-na.omit(train)
summary(train1)

#Converting appropriate variables to categorical variables 
train1$Survived<-as.factor(train1$Survived)
train1$Pclass<-as.factor(train1$Pclass)
train1$Sex<-as.factor(train1$Sex)
train1$SibSp<-as.factor(train1$SibSp)
train1$Parch<-as.factor(train1$Parch)
train1$Embarked<-as.factor(train1$Embarked)
str(train1)

# Reflecting the changes in test data set
test$Pclass<-as.factor(test$Pclass)
test$Sex<-as.factor(test$Sex)
test$SibSp<-as.factor(test$SibSp)
test$Parch<-as.factor(test$Parch)
test$Embarked<-as.factor(test$Embarked)
str(test) 

# Baseline model accuracy
table(train1$Survived)
BA <- (549/(549+342))
BA

#Feature Engineering
# #Exploratroy analysis by plotting and Hypothesis testing
ggplot(train1,aes(x=Age))+geom_histogram(binwidth=10)

#Pclass and Survival
ggplot(train1,aes(Pclass,Survived,color=Survived)) + geom_jitter()+ggtitle("Passenger Class Vs. Survival")
ggplot(train1,aes(Pclass,..count..))+geom_bar(aes(fill=Survived),position="fill")+ggtitle("Passenger Class Vs. Survival")
chisq.test(train1$Pclass, train1$Survived)#PClass affects survival rate


# Age and Survival
ggplot(train1,aes(x=Age,y=Survived,color=Survived))+geom_point()
ggplot(train1,aes(Age,Survived,color=Survived))+geom_jitter()
t.test(train1$Age~train1$Survived)#Age affects survival rate marginally


# Gender and Survival
ggplot(train1,aes(Sex,Survived,color=Survived))+geom_jitter()
chisq.test(train1$Sex, train1$Survived)#Gender affects survival rate


# Presence of siblings and Survival
ggplot(train1,aes(SibSp,Survived,color=Survived)) + geom_jitter()
ggplot(train1,aes(SibSp,..count..))+geom_bar(aes(fill=Survived),position="fill")+ggtitle("SiblingSp Vs. Survival")
chisq.test(train1$SibSp, train1$Survived)#Not reliable result


# Presence of Parent/child for survival
ggplot(train1,aes(Parch,factor(Survived))) + geom_jitter()
ggplot(train1,aes(Parch,..count..))+geom_bar(aes(fill=Survived),position="fill")+ggtitle("Parch Vs. Survival")

ggplot(train1,aes(Cabin,factor(Survived))) + geom_jitter()
chisq.test(train1$Parch, train1$Survived)#not reliable result

# Fare and survival
ggplot(train1,aes(Fare,factor(Survived))) + geom_jitter()+facet_grid(.~Sex)
t.test(train1$Fare~train1$Survived)#Fare affects survival rate


#port of embarkment
ggplot(train1,aes(Survived,Embarked))+geom_jitter()
chisq.test(train1$Embarked, train1$Survived)#not reliable result
ggplot(train1,aes(Embarked,..count..))+geom_bar(aes(fill=Survived),position="fill")+ggtitle("Parch Vs. Survival")


#Plot with 3 Variables
ggplot(train1,aes(Sex,Survived,color=Survived))+geom_jitter()+facet_grid(.~Pclass)
ggplot(train1,aes(Sex,Survived,color=Survived))+geom_jitter()+facet_grid(.~Embarked)


#Based on the plots and hypothesis tested the 
#following variables are included in the model

# Formula for the model
formula<-Survived~Pclass+Sex+Age+Fare


# Training by Cross validation
n<-nrow(train1)
splitPlan<-kWayCrossValidation(n,3,NULL,NULL)
str(splitPlan)
k<-3
train1$pred.cv<-0
for (i in 1:k){
  split<-splitPlan[[i]]
  model<-glm(formula,data=train1[split$train,],family=binomial)
  train1$pred.cv[split$app]<-predict(model,newdata=train1[split$app,],type="response")
}
summary(model)
# Predicting for full training data
train1$predictions<-predict(model,newdata=train1,type="response")

# Gain curve plot of predictions vs actual data for training data
GainCurvePlot(train1,"pred.cv","Survived","GCplot_Cross Validation")
#GainCurvePlot(train1,"predictions","Survived","GCplot")



# Confusion matrix
cm<-(table(Actualvalue = train1$Survived, PredictedValue = train1$pred.cv>0.5))
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
pred2 <- prediction(train1$pred.cv, train1$Survived)
roc.perf <- performance(pred2, measure = "tpr", x.measure = "fpr")
plot(roc.perf,colorize=TRUE,print.cutoffs.at=seq(0.1,by=0.1))



#prediction on test data
test$predictions<-predict(model,newdata=test,type="response")
str(test)
write.csv(test,file="Results.csv")