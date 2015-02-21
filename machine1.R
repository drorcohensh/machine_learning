machine1<- function() {

setwd("D:/R/coursera/coursera practical machine learning/assignment")  #set working directory

train<- read.csv("pml-training.csv")          #load data from csv files
test<- read.csv("pml-testing.csv")

nums <- sapply(test, is.numeric)      ## getting the column names of actual data in the                 
                                                  ## test data set              
numtest<- test[ , nums]
Testnames<-colnames(numtest)          ## setting the train data set with the relevant columns
Testnames[3]<-"classe"
Testnames[4]<-"user_name"
varnames<-Testnames[3:57]
rel_train <- train[ ,which(names(train) %in% varnames)]

library(e1071)
library(caret)
library(randomForest)
set.seed(8484)
InTrain<- createDataPartition(y=rel_train$classe, p=0.7, list=FALSE)
training<-rel_train [InTrain,]
testing<-rel_train [-InTrain,]

rf_model<-train(classe ~.,data=training,method="rf", trControl=trainControl(method="cv",number=5), prox=TRUE,allowParallel=TRUE)
rf_model

predictions <- predict(rf_model, newdata = testing)

confusionMatrix(data = predictions , testing$classe)


predictedAnswers<-predict(rf_model, newdata = test)
 
ans<- as.character(predictedAnswers)


pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(ans)
}