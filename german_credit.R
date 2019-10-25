#Machine Learning-German credit:Predicting whether person is eligibe for loan or not.
data=read.csv("gdata.csv",header=TRUE,stringsAsFactors =T)
head(data)
str(data)
data$Creditability=as.factor(data$Creditability)
id=sample(2,nrow(data),prob=c(.7,.3),replace=TRUE)
train=data[id==1,]
test=data[id==2,]
nrow(train)
nrow(test)
edit(train)
library(rpart)
m=rpart(Creditability~.,data=train,method="class",control=rpart.control(minsplit=20,
        minbucket=7,maxdepth=10,usesurrogate = 2,xval=10))
pre=predict(m,test)
pre
plot(m)
text(m)
library(RColorBrewer)
library(rpart.plot)
library(rattle)
fancyRpartPlot(m)
printcp(m)
bestcp=m$cptable[which.min(m$cptable[,"xerror"]),"CP"]
bestcp
pruned=prune(m,cp=bestcp)
fancyRpartPlot(pruned)
################Confusion matrix###########################
t=table(train$Creditability,predict(pruned,type="class"))
prop.table(table(train$Creditability,predict(pruned,type="class")))
rownames(t)=paste("Actual",rownames(t),sep=":")
colnames(t)=paste("predicted",colnames(t),sep=":")
t
prop.table(t)

accuracy=sum(diag(t))/sum(t)###on traning data
accuracy
t=predict(m,test,type="class")
s=prop.table(table(t,test$Creditability))
s
accuracy=sum(diag(s))/sum(s)  ####on test data
accuracy
#########################ROC CURVE###############################
for_auc=predict(pruned,test,type="prob")
library(pROC)
a=auc(test$Creditability,for_auc[,2])
a              #Ex:90-100,Good:80-90,fair:70-80,poor:60-70,Fail:50-60
plot(roc(test$Creditability,for_auc[,2]))
###########################Gini Coeff################################
2*a-1     # above 60% is good value
############################Random Forest#############
train$Creditability = as.factor(train$Creditability)
library(randomForest)
set.seed(71) 
rf <-randomForest(Creditability~.,data=train, ntree=500) 
rf
importance(rf)
varImpPlot(rf)
q=predict(rf,test,type="prob")
library(pROC)
x=auc(test$Creditability,q[,2])
x
plot(roc(test$Creditability,q[,2]))

