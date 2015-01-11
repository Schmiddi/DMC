# The caret package is used (http://topepo.github.io/caret/index.html)
#install.packages("caret")
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
install.packages("e1071")
library(e1071)

set.seed(42) # do NOT CHANGE this seed
# 2 x 5-fold cross validation
#fitCtrl = trainControl(method="repeatedcv", number=5, repeats=2)
# 2 x 10-fold cross validation
fitCtrl = trainControl(method="repeatedcv", number=10, repeats=2)
#fitCtrl = trainControl(method="repeatedcv", number=5, repeats=2, classProbs=TRUE, summaryFunction=twoClassSummary)

# training a decision tree model using the metric "Accuracy"
model = train(formula_with_most_important_attributes, data=training_data, method="J48", trControl=fitCtrl, metric="Accuracy", tuneGrid=data.frame(C=c(0.1, 0.2, 0.3)))
#model = train(formula_with_most_important_attributes, data=training_data, method="J48", trControl=fitCtrl, metric="ROC")

# Show results and metrics
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