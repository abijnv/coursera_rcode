coursera_rcode

###read train data#
traininput<-read.csv("E:\\coursera\\pml-training.csv")
dim(traininput)

#understanding train data#
str(traininput)
str(traininput[,150:160])
classe_table<-table(traininput$classe)
barplot(classe_table,main="Classe_count",xlab ="Classe",ylab="Count",col="blue")
traininput[,1]<-NULL
str(traininput)
nullcount<-colSums(is.na(traininput))
barplot(nullcount,main="nullcount",xlab ="variablenames",ylab="nullcount",col="blue")
plot(nullcount)
variable_withnull<-which(nullcount>0)
plot(variable_withnull)
table(variable_withnull)
barplot(variable_withnull,main="nullvar",xlab 
="variablenames",ylab="nullcount",col="blue")

##taking count of uniue values for every variable###
lu<-NULL
for( i in 1:ncol(traininput))
{
  lu[i]<-length(unique(traininput[,i]))
}
lu
names(traininput)
variable_u<-cbind(names(traininput),lu)
str(variable_u)
variable_u<-as.data.frame(variable_u)

###reading testinput###
testinput<-read.csv("E:\\coursera\\pml-testingna.csv")
testinput$X<-NULL
testinput$problem_id<-NULL

###taking null count of every column in testdata###
test_nullcount<-colSums(is.na(testinput))
test_withnull<-subset(test_nullcount,test_nullcount>0)

colnamesnotnull
for(i in 1:ncol(testinput))
{
  test_nullcount<-colSums(is.na(testinput))
  colnamesnull<-names(test_withnull)
}

###considering only non null variables in test data(only around 58 variables are 

considred for model building)####
test_nonull<-subset(test_nullcount,test_nullcount<1)
testnonull<-names(test_nonull)
testp<-testinput[,testnonull]

####taking same variables in trainingdata also##
traindata<-traininput[,testnonull]
colnames(traindata)[colnames(traindata)=="traininput$classe"] <- "classe"
str(traindata)

###modelling using caret package###
library(caret)
set.seed(1127)
###split training data into training and testing data set####
inTraining <- createDataPartition(traindata$classe, p = .75, list = FALSE)
trainingsample <- traindata[ inTraining,]
testingsample  <- traindata[-inTraining,]

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

##using gbm model##
set.seed(2029)
gbmFit1 <- train(classe ~ ., data = trainingsample,
                 method = "gbm",
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)

gbmFit1

###predicting the gbm fit model on sample test set obtained from the training partion 
and checking accuracy by confusion matrix####

testingsamplee<-testingsample[,1:58]
tespred<-predict(gbmFit1,newdata=testingsamplee)
confmatrix<-table( tespred,testingsample$classe)
confmatrix

###predicting the gbm fit model on original testing data###
testset_prediction<-predict(gbmFit1,newdata=testp)

###writing the result to a csv file###
write.csv(testset_prediction,"testset_prediction.csv")

