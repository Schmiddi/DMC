library(caret)
source("collapse_country_to_region.r")
source("collapse_country_to_status.r")
file <- "TrainingData.csv"

training_data <- read.table(file, sep = ",", header=TRUE)
test_data <- read.table("TestData.csv", sep = ",", header=TRUE)

training_data <- collapse_country_to_status(training_data)
table(training_data$status)

test_data <- collapse_country_to_status(test_data)
table(test_data$status)




