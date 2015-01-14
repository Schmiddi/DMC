# The caret package is used (http://topepo.github.io/caret/index.html)
#install.packages("caret")
#install.packages("klaR") # Needed for naive bayes
#install.packages("RWeka") # Needed for j48
install.packages("kernlab") # Needed for linear svm
library(caret)
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

nb <- function(attr, X) {
  fitCtrl <- trainControl(method="repeatedcv", number=10, repeats=2)
  mod <- train(attr, data=X, method="nb", trControl=fitCtrl, metric="Accuracy")
  return (mod)
}

svmLinear <- function(attr, X) {
  fitCtrl <- trainControl(method="repeatedcv", number=10, repeats=2)
  mod <- train(attr, data=X, method="svmLinear", trControl=fitCtrl, metric="Accuracy",tuneGrid=data.frame(C=c(2^(-5))))
  return (mod)  
}


# Show results and metrics
 model <- j48(formula_with_most_important_attributes, training_data)
model = nb(formula_with_most_important_attributes, training_data)
model = svmLinear(formula_with_most_important_attributes, training_data)
model
model$results

# Show decision tree
model$finalModel

# Show confusion matrix (in percent)
confusionMatrix(model)


######################################################
# 5. Predict Classes in Test Data
#prediction_classes = predict.train(object=model, newdata=test_data, na.action=na.pass)
#predictions = data.frame(id=test_data$ID, prediction=prediction_classes)
#predictions


######################################################
# 6. Export the Predictions
#write.csv(predictions, file="predictions_group_name_number.csv", row.names=FALSE)