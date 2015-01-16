# The caret package is used (http://topepo.github.io/caret/index.html)
#install.packages("caret")
library(caret)
#install.packages("FSelector")
library(FSelector)


# For reasons of traceability you must use a fixed seed
set.seed(42) # do NOT CHANGE this seed


###################################################
#### 1. Load & Explore the Training Data Set   ####
###################################################

training_data = read.csv("TrainingData.csv", sep=",")
test_data = read.csv("TestData.csv", sep=",")



#############################
##### Data Preparation  #####
#############################

training_data$id=NULL

### Review final structure
str(training_data)
str(test_data)


table(training_data$income)

#############################################
############# Feature Selection #############
#############################################

# Calculate weights for the attributes using Info Gain and Gain Ratio
weights_info_gain = information.gain(reorder ~ ., data=training_data)
#weights_info_gain
weights_gain_ratio = gain.ratio(reorder ~ ., data=training_data)
#weights_gain_ratio
# Select the 10 most important attributes based on Gain Ratio
most_important_attributes <- cutoff.k(weights_gain_ratio, 10)


source("DataVisualisation.r")
scatter(training_data[,c(most_important_attributes,"reorder")])

#most_important_attributes
formula_with_most_important_attributes <- as.simple.formula(most_important_attributes, "reorder")
#formula_with_most_important_attributes
