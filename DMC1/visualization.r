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

# Show the structure
str(training_data)

# Size
nrow(training_data)
ncol(training_data)

# Show the first and last rows
head(training_data)
tail(training_data)

# Show columns with missing values
colSums(is.na(training_data))

# Explore the class column
table(training_data$country)

# Explore the price column
mean(training_data$price)
# mean without N/A values
mean(training_data$price, na.rm=TRUE)

aggregate(x=training_data$price, by=list(training_data$a9), FUN=mean, na.rm=TRUE)

# Relationship
---------------
hist(training_data$relationship,frequency=TRUE)
table(training_data$relationship)
range(training_data$relationship)

hist(test_data$relationship,frequency=TRUE)
table(test_data$relationship)
range(test_data$relationship)

# Origin
---------------
hist(training_data$origin, breaks=5)
table(training_data$origin)
mode(training_data$origin)

hist(test_data$origin, breaks=5)
table(test_data$origin)
mode(test_data$origin)

# gender
---------------
hist(training_data$gender, breaks=2)
table(training_data$gender)

hist(test_data$gender, breaks=2)
table(test_data$gender)

# gain
---------------
hist(training_data$gain)
table(training_data$gain)
summary(training_data$gain)

hist(test_data$gain)
table(test_data$gain)
summary(test_data$gain)

# loss
---------------
hist(training_data$loss)
table(training_data$loss)
summary(training_data$loss)

hist(test_data$loss)
table(test_data$loss)
summary(test_data$loss)

# hours_weekly
---------------
hist(training_data$hours_weekly)
table(training_data$hours_weekly)
summary(training_data$hours_weekly)

hist(test_data$hours_weekly)
table(test_data$hours_weekly)
summary(test_data$hours_weekly)

# country
---------------
hist(training_data$country)
training_data[training_data$country=="?", ]
table(training_data$country)

hist(test_data$country)
training_data[test_data$country=="?", ]
table(test_data$country)

# income
---------------
hist(training_data$income)
table(training_data$income)

hist(test_data$income)
table(test_data$income)

boxplot(training_data$price ~ training_data$a9)
table(training_data$country)
