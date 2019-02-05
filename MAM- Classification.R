
#Loading the Data
train.raw=read.csv(file.choose())#,na.strings=c(""))#loandata file
dim(train.raw)
names(train.raw)
head(train.raw)
str(train.raw)
#Data Cleaning
#Checking the Missing Value
sum(complete.cases(train.raw))
sapply(train.raw,function(x) sum(is.na(x)))
#str(loan.raw)
#filling the missing values
train.raw$Age [is.na(train.raw$Age)] = round(mean(train.raw$Age,na.rm=TRUE))

# Removing the un used attributes 
trainorig=train.raw[,-c(1,4,6,9,10,11)]
names(trainorig)
str(trainorig)
head(trainorig)
trainorig$Sex= as.numeric(trainorig[,3])
trainorig$Embarked= as.numeric(trainorig[,6])
#trainorig$Sex=factor(trainorig$Sex,levels=c("male","female"),labels=c(0,1))
#trainorig$Embarked = factor(trainorig$Embarked, levels = c("S","C","Q"), labels = c(0,1,2))
#Data preparation
res=cor(trainorig[,1:6])
round(res,2)
#Removing Experience and CCAVg applicantIncome
#loan=loan[,-c(2,5)]
#names(loan)
#checking the correlation between independent and dependednt variables
cor(trainorig[,1:6],trainorig$Survived)
#loan=loan[,-c(1,3)]
#str(loan)
trainorig$Survived=factor(trainorig$Survived,levels=c(0,1),labels=c("No","Yes"))
summary(trainorig)
#Dividing the data
d = sort(sample(nrow(trainorig), nrow(trainorig)*.7))
train<-trainorig[d,]
test<-trainorig[-d,]
head(test)
sink("test.csv", append = FALSE)
dim(train)
dim(test)
# Classification Algorithms
#Decission Tree
library("rpart")
library("rpart.plot")
model.tree <- rpart (Survived~.,data=train,method="class")
summary(model.tree)
prp(model.tree, cex = 0.8, extra = 2)
plotcp(model.tree)
printcp(model.tree)
pTree<- prune(model.tree,0.042)
prp(pTree,  cex = 0.8, extra = 2)
names(test)
#predicting performance
pred.tree <- predict(pTree,test, type="class")
#Performance Matrix for Decission Tree
conf.matrix = table(pred.tree,test$Survived)
#rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ":")
#colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix), sep = ":")
print(conf.matrix)
performance=function(m)
{ 
  #accuracy=0;Sensitivity=0;specificity=0;Time=0
  accuracy = sum(diag(conf.matrix)) / sum(conf.matrix)
  #accuracy[1]
  Sensitivity= conf.matrix[1,1]/sum(conf.matrix[,1])
  # Sensitivity[1]
  specificity = conf.matrix[2,2] / sum(conf.matrix[,2] )
  #specificity[1]
  t=system.time(pred.tree <- predict(pTree,test, type="class"))
  class(t)
  Time=t[3]
  PerformM=c(accuracy,Sensitivity, specificity,Time)
  return(PerformM)
}
Perform_DT=performance(conf.matrix)
#print(Perform_DT)
#Naive Bayes
library(e1071)
model.naiveBayes <- naiveBayes(Survived ~ ., data = train, laplace = 3)
pred.naiveBayes <- predict(model.naiveBayes, test,type="class")
head(pred.naiveBayes)
#Performance Matrix for Naive Bayes
conf.matrix = table(test$Survived,pred.naiveBayes)
#rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ":")
#colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix), sep = ":")
print(conf.matrix)
Perform_NB=performance(conf.matrix)
#SVM
model.svm=svm(formula =Survived ~ ., data = train)
print(model.svm)
pred.svm<- predict(model.svm, test)
plot(pred.svm)

#Performance Matrix for SVM
conf.matrix = table(test$Survived,pred.svm)
#rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ":")
#colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix), sep = ":")
print(conf.matrix)
Perform_SVM=performance(conf.matrix)
#Logistic Regression 

model.glm <- glm (Survived~ ., data =train, family = binomial)
summary(model.glm)
predict <- predict(model.glm,test, type = 'response')
pred= ifelse(predict > 0.5,1,0)
#Performance Matrix for Logistic Regression
conf.matrix= table(test$Survived, pred)
#rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ":")
#colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix), sep = ":")
print(conf.matrix)
Perform_LR=performance(conf.matrix)
#Performance of Classification Methods: A comparision
P_matrics=matrix(c(Perform_DT,Perform_NB,Perform_SVM,Perform_LR),4,b=1)

colnames(P_matrics)=c("accuracy","Sensitivity","specificity", "Ex. Time")
rownames(P_matrics)=c("D.Tree","Naive_Bayes", "SVM", "Log.regression")
print(P_matrics)
F_matrics=P_matrics[,-4]
apply(F_matrics,1,mean)

