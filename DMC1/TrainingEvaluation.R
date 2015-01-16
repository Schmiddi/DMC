# The caret package is used (http://topepo.github.io/caret/index.html)
#install.packages("caret")
#install.packages("klaR") # Needed for naive bayes
#install.packages("RWeka") # Needed for j48
#install.packages("LogicReg") # Needed for logistic regression
install.packages("kernlab") # Needed for linear svm
library(caret)
#install.packages("adabag") # Needed for ada boosting
# For reasons of traceability you must use a fixed seed
set.seed(42) # do NOT CHANGE this seed


##########################
#### 3. Prepare Data  ####
##########################

source("DataPreparation.R")

###################################
#### 4. Training & Evaluation  ####
###################################
#install.packages("e1071")
library(e1071)

set.seed(42) # do NOT CHANGE this seed
j48 <- function(attr, X){
  # 2 x 5-fold cross validation
  #fitCtrl = trainControl(method="repeatedcv", number=5, repeats=2)
  # 2 x 10-fold cross validation
  
  #fitCtrl = trainControl(method="repeatedcv", number=5, repeats=2, classProbs=TRUE, summaryFunction=twoClassSummary)
  fitCtrl = trainControl(method="repeatedcv", number=10, repeats=2)
  
  # training a decision tree model using the metric "Accuracy"
  m = train(attr, data=X, method="J48", trControl=fitCtrl, metric="Accuracy", tuneGrid=data.frame(C=c(0.1, 0.2, 0.3)))
  #model = train(formula_with_most_important_attributes, data=training_data, method="J48", trControl=fitCtrl, metric="ROC")
  return (m)
}

# Naive Bayes
nb <- function(attr, X) {
  set.seed(42) # do NOT CHANGE this seed
  fitCtrl <- trainControl(method="repeatedcv", number=10, repeats=2)
  mod <- train(attr, data=X, method="nb", trControl=fitCtrl, metric="Accuracy")
  return (mod)
}

svmLinear <- function(attr, X) {
  set.seed(42) # do NOT CHANGE this seed
  fitCtrl <- trainControl(method="repeatedcv", number=10, repeats=1)
  mod <- train(attr, data=X, method="svmLinear", trControl=fitCtrl, metric="Accuracy",tuneGrid=data.frame(C=c(1,2^5,2^10)))
  return (mod)  
}

# ada
ada2 <- function(attr, X){
  set.seed(42) # do NOT CHANGE this seed
  fitCtrl <- trainControl(method="repeatedcv", number=10, repeats=1)
  mod <- train(attr, data=X, method="AdaBoost.M1", trControl=fitCtrl, metric="Accuracy", preProc = c("center","scale"))
  return(mod)
}
adaBest <- function(attr, X){
  set.seed(42) # do NOT CHANGE this seed
  fitCtrl <- trainControl(method="repeatedcv", number=10, repeats=1)
  mod <- train(attr, data=X, method="AdaBoost.M1", trControl=fitCtrl, metric="Accuracy", preProc = c("center","scale"), tuneGrid=data.frame(coeflearn="Freund",maxdepth=2,mfinal=150))
  return(mod)
}

# knn
knn <- function(attr, X) {
  set.seed(42) # do NOT CHANGE this seed
  fitCtrl <- trainControl(method="repeatedcv", number=10, repeats=1)
  mod <- train(attr, data=X, method="knn", trControl=fitCtrl, metric="Accuracy",tuneGrid=data.frame(k=c(55,155,255)))
  return (mod)  
}

# Logistic Regression
logreg <- function(attr, X) {
  set.seed(42) # do NOT CHANGE this seed
  fitCtrl <- trainControl(method="repeatedcv", number=10, repeats=1)
  mod <- train(attr, data=X, method="logreg", trControl=fitCtrl, metric="Accuracy" ,tuneGrid=data.frame(treesize=c(1,10), ntrees=c(1,10)) )
  return (mod)
}

?train

# Show results and metrics
 model <- j48(formula_with_most_important_attributes, training_data)
model = nb(formula_with_most_important_attributes, training_data)
model = svmLinear(formula_with_most_important_attributes, training_data)
model = ada2(formula_with_most_important_attributes, training_data)
model = adaBest(formula_with_most_important_attributes, training_data)
modelKnn = knn(formula_with_most_important_attributes, training_data)
modelKnn
modelLogreg = logreg(formula_with_most_important_attributes, training_data)
modelLogreg
model
model$results

# Show decision tree
model$finalModel

# Show confusion matrix (in percent)
confusionMatrix(model)


######################################################
# 5. Predict Classes in Test Data
prediction_classes = predict.train(object=model, newdata=test_data, na.action=na.pass)
predictions = data.frame(id=test_data$id, prediction=prediction_classes)
predictions


######################################################
# 6. Export the Predictions
write.csv(predictions, file="predictions_DropDatabase_2.csv", row.names=FALSE)
