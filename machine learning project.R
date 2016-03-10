library(caret)
training <- read.csv("./Coursera/pml-training.csv", na.strings = c("NA",""))
testing <- read.csv("./Coursera/pml-testing.csv",  na.strings =c("NA",""))

dim(training)
#Remove non-explanatory variables
uselessColumns<-grep("X|user_name|timestamp|window",names(training))
training<-training[,-uselessColumns]

# remove near zero variance columns
nzvColumns <- nearZeroVar(training)
training<-training[,-nzvColumns]

#remove columns with NAs.
NAcolumns <- sapply(training, function(x){as.logical(sum(is.na(x)))})
training<-training[,!NAcolumns]

#Assemble Training/Validation Sets
to_train <- createDataPartition(y=training$classe, p=.8, list=F)

#Split into Training and Validation
training_set <- training[to_train,]
validate_set <- training[-to_train,]

#Attempt Classification with Random Forest and Boosting
model1 <- train(classe ~ ., method="rf",  trControl=trainControl(method="cv"), data=training_set)
model2 <- train(classe ~ ., method="gbm", trControl=trainControl(method="cv"), data=training_set)

#Validate Model Output - RF
rf_validation_test <- predict(model1, validate_set)
confusionMatrix(rf_validation_test,validate_set$classe)
#Validate Model Output - Gradient Boosting Machine
gbm_validation_test <- predict(model2, validate_set)
confusionMatrix(gbm_validation_test,validate_set$classe)

plot(model1)
plot(model2)

VariableImportance<-varImp(model1)
plot(VariableImportance)


rfPredict<-predict(model1,testing)
gbmPredict<-predict(model2,testing)

mean(rfPredict==gbmPredict)

rfPredict