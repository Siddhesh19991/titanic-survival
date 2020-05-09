setwd("~/Downloads/kaggle")
titanic.train<-read.csv(file="train.csv",stringsAsFactors = FALSE,header = TRUE)
titanic.test<-read.csv(file="test.csv",stringsAsFactors = FALSE,header = TRUE)

titanic.train$check<-TRUE
titanic.test$check<-FALSE
titanic.test$Survived<-NA
titanic.final<-rbind(titanic.train,titanic.test)
titanic.final$Embarked[c(62,830)]<-"S"
median<-median(titanic.final$Age,na.rm=TRUE)
a<-which(is.na(titanic.final$Age))
titanic.final$Age[a]<-28

median2<-median(titanic.final$Fare,na.rm=TRUE)
titanic.final$Fare[which(is.na(titanic.final$Fare))]<-median2

titanic.final$Sex<-as.factor(titanic.final$Sex)
titanic.final$Pclass<-as.factor(titanic.final$Pclass)
titanic.final$Sex<-as.factor(titanic.final$Sex)
titanic.final$SibSp<-as.factor(titanic.final$SibSp)
titanic.final$Parch<-as.factor(titanic.final$Parch)
titanic.final$Embarked<-as.factor(titanic.final$Embarked)


titanic.train<-titanic.final[titanic.final$check==TRUE,]
titanic.test<-titanic.final[titanic.final$check==FALSE,]

titanic.train$Survived<-as.factor(titanic.train$Survived)

machine<-"Survived~ Pclass + Sex + SibSp + Parch + Fare + Embarked "
machine1<-as.formula(machine)
install.packages("randomForest")
library(randomForest)
model<-randomForest(formula=machine1,data=titanic.train,ntree=500,mtry=3,nodesize=0.01*nrow(titanic.test))
answers<-predict(model,titanic.test)


PassengerId<-titanic.test$PassengerId

result<-as.data.frame(PassengerId)
result$Survived<-answers


write.csv(result,file="titanic.csv",row.names = FALSE)
