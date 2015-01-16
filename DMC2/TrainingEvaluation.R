# The caret package is used (http://topepo.github.io/caret/index.html)
#install.packages("caret")
#install.packages("klaR") # Needed for naive bayes
#install.packages("RWeka") # Needed for j48
# install.packages("kernlab") # Needed for linear svm
# install.packages("k") # Needed for knn
# install.packages("randomForest")
library(caret)
# For reasons of traceability you must use a fixed seed
set.seed(42) # do NOT CHANGE this seed
library(randomForest)

##########################
#### 3. Prepare Data  ####
##########################

source("DataPreparation.R")

###################################
#### 4. Training & Evaluation  ####
###################################
#install.packages("e1071")
library(e1071)

j48 <- function(attr, X){
  set.seed(42) # do NOT CHANGE this seed
  fitCtrl = trainControl(method="repeatedcv", number=10, repeats=2)
  m = train(attr, data=X, method="J48", trControl=fitCtrl, metric="Accuracy", tuneGrid=data.frame(C=c(0.1, 0.2, 0.3)))
  return (m)
}

nb <- function(attr, X) {
  set.seed(42)
  fitCtrl <- trainControl(method="repeatedcv", number=10, repeats=1)
  mod <- train(attr, data=X, method="nb", trControl=fitCtrl, metric="Accuracy")
  return (mod)
}

svmLinear <- function(attr, X) {
  set.seed(42)
  fitCtrl <- trainControl(method="repeatedcv", number=10, repeats=1)
  mod <- train(attr, data=X, method="svmLinear", trControl=fitCtrl, metric="Accuracy",tuneGrid=data.frame(C=c(2^(-5),2^(-4),2^(-3),2^(-2),2^(-1),2^(0),2^(1),2^(2),2^(3),2^(4))))
  return (mod)  
}

svmRadialCost <- function(attr, X) {
  set.seed(42)
  fitCtrl <- trainControl(method="repeatedcv", number=10, repeats=1)
  mod <- train(attr, data=X, method="svmRadialCost", trControl=fitCtrl, metric="Accuracy",tuneGrid=data.frame(C=c(2^(-5))))#,2^(-4),2^(-3),2^(-2),2^(-1),2^(0),2^(1),2^(2),2^(3),2^(4))))
  return (mod)  
}

knn <- function(attr, X) {
  set.seed(42)
  fitCtrl <- trainControl(method="repeatedcv", number=10, repeats=1)
  mod <- train(attr, data=X, method="knn", trControl=fitCtrl, metric="Accuracy",
               tuneGrid=data.frame(C=c(5,15,25))
  return (mod)  
}

randomForest <- function(attr, X) {
  set.seed(42)
  fitCtrl <- trainControl(method="repeatedcv", number=10, repeats=1)
  mod <- train(attr, data=X, method="rf", trControl=fitCtrl, metric="Accuracy"
               ,tuneGrid = data.frame(.mtry=sqrt(ncol(X))))
  return (mod)
}

adaBest <- function(attr, X){
  set.seed(42) # do NOT CHANGE this seed
  fitCtrl <- trainControl(method="repeatedcv", number=10, repeats=1)
  mod <- train(attr, data=X, method="AdaBoost.M1", trControl=fitCtrl, metric="Accuracy", preProc = c("center","scale"), tuneGrid=data.frame(coeflearn="Freund",maxdepth=2,mfinal=150))
  return(mod)
}

formula_with_most_important_attributes
# Show results and metrics
#model <- j48(formula_with_most_important_attributes, training_data)
#model = nb(formula_with_most_important_attributes, training_data)
#model = svmLinear(formula_with_most_important_attributes, training_data)
#model = randomForest(formula_with_most_important_attributes, training_data)
#model = svmRadialCost(formula_with_most_important_attributes, training_data)
model <- adaBest(formula_with_most_important_attributes, training_data)
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
table (predictions$prediction)

######################################################
# 6. Export the Predictions
write.csv(predictions, file="predictions_DropDatabase_1.csv", row.names=FALSE)
