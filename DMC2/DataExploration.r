# Load Data
# -------------------------------
training_data = read.csv("TrainingData.csv", sep=",")
test_data = read.csv("TestData.csv", sep=",")

############################################
#### 1. Explore the Training Data Set   ####
############################################
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

# Age
# -------------------------------
hist(training_data$age)
hist(test_data$age)
boxplot(training_data$price ~ training_data$a9)

range(training_data$age)
range(test_data$age)
table(training_data$age, useNA="always")
table(test_data$age, useNA="always")
summary(training_data$age)
summary(test_data$age)

# Warkclass
# -------------------------------
table(training_data$workclass, useNA="always")
table(test_data$workclass, useNA="always")

# Rating
# -------------------------------
range(training_data$rating)
range(test_data$rating)
hist(training_data$rating)
hist(test_data$rating)

# Education
# -------------------------------
table(training_data$education, useNA="always")
table(test_data$education, useNA="always")
table(training_data$edu, useNA="always")
table(test_data$edu, useNA="always")

# Family_status
# -------------------------------
range(training_data$family_status)
range(test_data$family_status)
table(training_data$family_status, useNA="always")
table(test_data$family_status, useNA="always")

# Occupation
# -------------------------------
table(training_data$occupation, useNA="always")
table(test_data$occupation, useNA="always")

# Income
# -------------------------------
table(training_data$income, useNA="always")
table(test_data$income, useNA="always")

# Workclass vs Occupation
# -------------------------------
?chisq.test
chisq.test(training_data$occupation, training_data$income)
# X-squared = 5216.337, df = 98, p-value < 2.2e-16

head(test_data)

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