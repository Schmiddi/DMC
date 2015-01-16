# The caret package is used (http://topepo.github.io/caret/index.html)
#install.packages("caret")
library(caret)
source("collapse_country_to_region.r")
source("collapse_country_to_status.r")
source("remove_leading_whitespace_factor.R")
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

#------------ id ------------#
# Remove column
training_data$id = NULL 
#test_data$id = NULL 

#------------ workclass ------------#
# Remove leading whitepace from factors
training_data$workclass <- remove_leading_whitespace_factor(training_data$workclass)
test_data$workclass <- remove_leading_whitespace_factor(test_data$workclass)
# Set ? to NA
training_data$workclass[training_data$workclass=="?"] <- NA
test_data$workclass[test_data$workclass=="?"] <- NA
# Drop unused levels
training_data$workclass <- droplevels(training_data$workclass)
test_data$workclass <- droplevels(test_data$workclass)

#------------ education ------------#
# Remove leading whitepace from factors
training_data$education <- remove_leading_whitespace_factor(training_data$education)
test_data$education <- remove_leading_whitespace_factor(test_data$education)
# Make education ordnered
EDUCATION_ORDER = c("Preschool", "1st-4th", "5th-6th", "7th-8th", "9th", "10th", "11th", "12th", "HS-grad", "Some-college", "Assoc-voc", "Assoc-acdm","Bachelors", "Masters", "Prof-school", "Doctorate")
training_data$education  = ordered(training_data$education , levels=EDUCATION_ORDER)
test_data$education = ordered(test_data$education, levels=EDUCATION_ORDER)

#------------ edu ------------#
# Remove edu (redundand with education)
training_data$edu = NULL
test_data$edu = NULL

#------------ family_status ------------#
FAMILY_STATUS = c("divorced", "married", "never-married", "separated", "widowed")
training_data$family_status = factor(training_data$family_status, levels=1:5, labels=FAMILY_STATUS)
test_data$family_status = factor(test_data$family_status, levels=1:5, labels=FAMILY_STATUS)

#------------ occupation ------------#
# Remove leading whitepace from factors
training_data$occupation <- remove_leading_whitespace_factor(training_data$occupation)
test_data$occupation <- remove_leading_whitespace_factor(test_data$occupation)
# Set ? to NA
training_data$occupation[training_data$occupation=="?"] <- NA
test_data$occupation[test_data$occupation=="?"] <- NA
# Drop unused levels
training_data$occupation <- droplevels(training_data$occupation)
test_data$occupation <- droplevels(test_data$occupation)

#------------ relationship ------------#
# Remove leading whitepace from factors
training_data$relationship <- remove_leading_whitespace_factor(training_data$relationship)
test_data$relationship <- remove_leading_whitespace_factor(test_data$relationship)
# ordinal ? wirklich ?

#------------ origin ------------#
# ordinal? nicht eher nominal?
# Make ordinal
ORIGIN = c("native american", "asian", "black", "other", "white")
training_data$origin = factor(training_data$origin, levels=1:5, labels=ORIGIN)
test_data$origin = factor(test_data$origin, levels=1:5, labels=ORIGIN)

#------------ gender ------------#
# ordinal? nicht eher nominal?
# Make ordinal
GENDER = c("female", "male")
training_data$gender = factor(training_data$gender, levels=1:2, labels=GENDER)
test_data$gender = factor(test_data$gender, levels=1:2, labels=GENDER)

#------------ gain ------------#
#------------ loss ------------#
#------------ hours_weekly ------------#

#------------ country ------------#
# Remove leading whitepace from factors
training_data$country <- remove_leading_whitespace_factor(training_data$country)
test_data$country <- remove_leading_whitespace_factor(test_data$country)
# Set ? to NA
training_data$country[training_data$country=="?"] <- NA
test_data$country[test_data$country=="?"] <- NA
# Drop unused levels (?)
training_data$country <- droplevels(training_data$country)
test_data$country <- droplevels(test_data$country)
# TO character
training_data$country=as.character(training_data$country)
test_data$country=as.character(test_data$country)

### Add column status based on country
training_data <- collapse_country_to_status(training_data)
test_data <- collapse_country_to_status(test_data)

# Set "South" to NA
# Was ist der Status "south" ???
training_data$status[training_data$status=="South"] <- NA
test_data$status[test_data$status=="South"] <- NA

# Convert "chr" to factor
training_data$status <- as.factor(training_data$status)
test_data$status <- as.factor(test_data$status)

# Convert factor to ordinal
STATUS = c("low", "lower" , "upper", "high")
training_data$status  = ordered(training_data$status , levels=STATUS)
test_data$status  = ordered(test_data$status , levels=STATUS)

### Add column region based on country
training_data <- collapse_country_to_region(training_data)
test_data <- collapse_country_to_region(test_data)

# Set "South" to NA
# Was ist der Status "south" ???
training_data$region[training_data$region=="South"] <- NA
test_data$region[test_data$region=="South"] <- NA

# Convert "chr" to factor
training_data$region <- as.factor(training_data$region)
test_data$region <- as.factor(test_data$region)


## Drop Country
training_data$country = NULL

training_data$family_status = NULL
test_data$family_status = NULL
### Review final structure
str(training_data)
str(test_data)


#-------------------------------------------#
#------------ Feature Selection ------------#
#-------------------------------------------#
# Calculate weights for the attributes using Info Gain and Gain Ratio
weights_info_gain = information.gain(income ~ ., data=training_data)
#weights_info_gain
weights_gain_ratio = gain.ratio(income ~ ., data=training_data)
#weights_gain_ratio
# Select the 10 most important attributes based on Gain Ratio
most_important_attributes <- cutoff.k(weights_gain_ratio, 10)
#most_important_attributes
formula_with_most_important_attributes <- as.simple.formula(most_important_attributes, "income")
#formula_with_most_important_attributes
