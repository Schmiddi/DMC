# The caret package is used (http://topepo.github.io/caret/index.html)
#install.packages("caret")
library(caret)
#install.packages("FSelector")
library(FSelector)
library(DMwR)


# For reasons of traceability you must use a fixed seed
set.seed(42) # do NOT CHANGE this seed


###################################################
#### 1. Load & Explore the Training Data Set   ####
###################################################

training_data = read.csv("TrainingData.csv", sep=",")
test_data = read.csv("TestData.csv", sep=",")

# Create a hold out set with 20% of samples
training_data <- training_data[sample(1:nrow(training_data)), ] # randomize samples
yes <- training_data[training_data$reorder == "yes",]
no <- training_data[training_data$reorder == "no",]
test_train <- rbind(yes[1:151, ], no[1:649, ])
training_data <- rbind(yes[152:nrow(yes), ], no[650:nrow(no), ])
test_train <- training_data[sample(1:nrow(test_train)), ] # randomize samples
training_data <- training_data[sample(1:nrow(training_data)), ] # randomize samples
names(test_train)[names(test_train)=="reorder"] <- "reorder_original"
test_train$reorder <- -1
#############################
##### Data Preparation  #####
#############################
#str(training_data)

#------------ id ------------#
# Remove column
training_data$id = NULL 
#test_data$id = NULL 

# saluation
#table(training_data$salutation, useNA="always")
#table(test_data$salutation, useNA="always")
SALUTATION = c("Ms.", "Mr.", "Company")
training_data$salutation = factor(training_data$salutation, levels=0:2, labels=SALUTATION)
test_train$salutation = factor(test_train$salutation, levels=0:2, labels=SALUTATION)
test_data$salutation = factor(test_data$salutation, levels=0:2, labels=SALUTATION)

# title
#table(training_data$title, useNA="always")
#table(test_data$title, useNA="always")
TITLE = c("not available", "available")
training_data$title = factor(training_data$title, levels=0:1, labels=TITLE)
test_train$title = factor(test_train$title, levels=0:1, labels=TITLE)
test_data$title = factor(test_data$title, levels=0:1, labels=TITLE)

# mail
#table(training_data$mail, useNA="always")
#table(test_data$mail, useNA="always")
MAIL=c("AOL","Arcor","Freenet","Google Mail", "GMX", "Hotmail", "online.de", "onlinehome.de", "T-Online", "web.de", "Yahoo (com)", "Yahoo (de)","others")
training_data$mail = factor(training_data$mail, levels=0:12, labels=MAIL)
test_train$mail = factor(test_train$mail, levels=0:12, labels=MAIL)
test_data$mail = factor(test_data$mail, levels=0:12, labels=MAIL)

#newsletter
#table(training_data$newsletter, useNA="always")
#table(test_data$newsletter, useNA="always")
NOYES = c("no", "yes")
training_data$newsletter = factor(training_data$newsletter, levels=0:1, labels=NOYES)
test_data$newsletter = factor(test_data$newsletter, levels=0:1, labels=NOYES)
test_train$newsletter = factor(test_train$newsletter, levels=0:1, labels=NOYES)

# model
#table(training_data$model, useNA="always")
#table(test_data$model, useNA="always")
MODEL = c("Model one", "Model two","Model three")
training_data$model = factor(training_data$model, levels=1:3, labels=MODEL)
test_data$model = factor(test_data$model, levels=1:3, labels=MODEL)
test_train$model = factor(test_train$model, levels=1:3, labels=MODEL)

# paymenttype
#table(training_data$paymenttype, useNA="always")
#table(test_data$paymenttype, useNA="always")
PAYMENTTYPE = c("Payment on invoice", "Cash payment", "Transfer from current account", "Transfer from credit card") 
training_data$paymenttype = factor(training_data$paymenttype, levels=0:3, labels=PAYMENTTYPE)
test_data$paymenttype = factor(test_data$paymenttype, levels=0:3, labels=PAYMENTTYPE)
test_train$paymenttype = factor(test_train$paymenttype, levels=0:3, labels=PAYMENTTYPE)

# deliverytype
#table(training_data$deliverytype, useNA="always")
#table(test_data$deliverytype, useNA="always")
DELIVERYTYPE = c("Dispatch", "Collection")
training_data$deliverytype = factor(training_data$deliverytype, levels=0:1, labels=DELIVERYTYPE)
test_data$deliverytype = factor(test_data$deliverytype, levels=0:1, labels=DELIVERYTYPE)
test_train$deliverytype = factor(test_train$deliverytype, levels=0:1, labels=DELIVERYTYPE)

# invoicepostcode
#table(training_data$invoicepostcode, useNA="always")
#table(test_data$invoicepostcode, useNA="always")
training_data$invoicepostcode = factor(training_data$invoicepostcode)
test_data$invoicepostcode = factor(test_data$invoicepostcode)
test_train$invoicepostcode = factor(test_train$invoicepostcode)

# delivpostcode
#table(training_data$delivpostcode, useNA="always")
#table(test_data$delivpostcode, useNA="always")
#training_data$delivpostcode = factor(training_data$delivpostcode)
#test_data$delivpostcode = factor(test_data$delivpostcode)
training_data$delivpostcode = NULL
test_data$delivpostcode = NULL
test_train$delivpostcode = NULL

# voucher
#table(training_data$voucher, useNA="always")
#table(test_data$voucher, useNA="always")
#NOYES = c("no", "yes")
training_data$voucher = factor(training_data$voucher, levels=0:1, labels=NOYES)
test_data$voucher = factor(test_data$voucher, levels=0:1, labels=NOYES)
test_train$voucher = factor(test_train$voucher, levels=0:1, labels=NOYES)

# advertisingdatacode
#table(training_data$advertisingdatacode, useNA="always")
#table(test_data$advertisingdatacode, useNA="always")
#NOYES = c("no", "yes")
tmp = as.character(training_data$advertisingdatacode)
tmp[tmp==""] = "no"
tmp[tmp!="no"] = "yes"
training_data$advertisingdatacode = factor(tmp)

tmp = as.character(test_data$advertisingdatacode)
tmp[tmp==""] = "no"
tmp[tmp!="no"] = "yes"
test_data$advertisingdatacode = factor(tmp)

tmp = as.character(test_train$advertisingdatacode)
tmp[tmp==""] = "no"
tmp[tmp!="no"] = "yes"
test_train$advertisingdatacode = factor(tmp)

# value
table(training_data$value, useNA="always")
#table(test_data$value, useNA="always")
#NOYES = c("no", "yes")
VALUE = c("lowest","low","mid","high","highest")
training_data$value  = factor(training_data$value ,levels=1:5, labels=VALUE)
test_data$value = factor(test_data$value, levels=1:5,labels=VALUE)
test_train$value = factor(test_train$value, levels=1:5,labels=VALUE)
#str(training_data)

# numberitems
#table(training_data$numberitems, useNA="always")
#table(test_data$numberitems, useNA="always")
# unchanged

# gift
#table(training_data$gift, useNA="always")
#table(test_data$gift, useNA="always")
#NOYES = c("no", "yes")
training_data$gift = factor(training_data$gift, levels=0:1, labels=NOYES)
test_data$gift = factor(test_data$gift, levels=0:1, labels=NOYES)
test_train$gift = factor(test_train$gift, levels=0:1, labels=NOYES)

# entry
#table(training_data$entry, useNA="always")
#table(test_data$entry, useNA="always")
ENTRY = c("shop", "partner")
training_data$entry = factor(training_data$entry, levels=0:1, labels=ENTRY)
test_data$entry = factor(test_data$entry, levels=0:1, labels=ENTRY)
test_train$entry = factor(test_train$entry, levels=0:1, labels=ENTRY)

# points
#table(training_data$points, useNA="always")
#table(test_data$points, useNA="always")
#NOYES = c("no", "yes")
#training_data$points = factor(training_data$points, levels=0:1, labels=NOYES)
#test_data$points = factor(test_data$points, levels=0:1, labels=NOYES)

training_data$points = NULL
test_data$points= NULL
test_train$points= NULL

# shippingcosts
#table(training_data$shippingcosts, useNA="always")
#table(test_data$shippingcosts, useNA="always")
#NOYES = c("no", "yes")
training_data$shippingcosts = factor(training_data$shippingcosts, levels=0:1, labels=NOYES)
test_data$shippingcosts = factor(test_data$shippingcosts, levels=0:1, labels=NOYES)
test_train$shippingcosts = factor(test_train$shippingcosts, levels=0:1, labels=NOYES)

# weight
#table(training_data$weight, useNA="always")
#table(test_data$weight, useNA="always")
# no thing changed

# remitted
#table(training_data$remitted, useNA="always")
#table(test_data$remitted, useNA="always")
# no thing changed

# cancelled
#table(training_data$cancelled, useNA="always")
#table(test_data$cancelled, useNA="always")
# no thing changed

# used
#table(training_data$used, useNA="always")
#table(test_data$used, useNA="always")

# reorder
#table(training_data$reorder, useNA="always")
#table(test_data$reorder, useNA="always")

### Review final structure
#str(training_data)
#str(test_data)


#-------------------------------------------#
#---------- Smote ------------------- ------#
#-------------------------------------------#
#yes <- training_data[training_data$reorder == "yes",]
#no <- training_data[training_data$reorder == "no",]
#no <- no[sample(1:nrow(no)), ]
#no <- no[2001:nrow(no),]
#training_data2 <- rbind(yes,no)
#training_data <- training_data2[sample(1:nrow(training_data2)), ]

training_data <- SMOTE(reorder ~ ., data=training_data, perc.over=200, perc.under=200, k=5)
training_data$numberitems = as.integer(training_data$numberitems)


### DATE STUFF

#------------ date ------------#
#table(training_data$date, useNA="always")
#table(test_data$date, useNA="always")
date_format = "%Y-%m-%d"
training_data$date = as.Date(training_data$date, date_format)
test_data$date = as.Date(test_data$date, date_format)
test_train$date = as.Date(test_train$date, date_format)


#creation_date
#table(training_data$creation_date, useNA="always")
#table(test_data$creation_date, useNA="always")
#date_format = "%Y-%m-%d"
training_data$creation_date = as.Date(training_data$creation_date, date_format)
test_data$creation_date = as.Date(test_data$creation_date, date_format)
test_train$creation_date = as.Date(test_train$creation_date, date_format)

# deliverydatepromised
#table(training_data$deliverydatepromised, useNA="always")
#table(test_data$deliverydatepromised, useNA="always")
#date_format = "%Y-%m-%d"
training_data$deliverydatepromised = as.Date(training_data$deliverydatepromised, date_format)
test_data$deliverydatepromised = as.Date(test_data$deliverydatepromised, date_format)
test_train$deliverydatepromised = as.Date(test_train$deliverydatepromised, date_format)

# deliverydatereal
#table(training_data$deliverydatereal, useNA="always")
#table(test_data$deliverydatereal, useNA="always")
#date_format = "%Y-%m-%d"
training_data$deliverydatereal = as.Date(training_data$deliverydatereal, date_format)
test_data$deliverydatereal = as.Date(test_data$deliverydatereal, date_format)
test_train$deliverydatereal = as.Date(test_train$deliverydatereal, date_format)


#-------------------------------------------#
#------------ Feature Selection ------------#
#-------------------------------------------#
# Calculate weights for the attributes using Info Gain and Gain Ratio
weights_info_gain = information.gain(reorder ~ ., data=training_data)
#weights_info_gain
weights_gain_ratio = gain.ratio(reorder ~ ., data=training_data)
#weights_gain_ratio
# Select the 10 most important attributes based on Gain Ratio
most_important_attributes <- cutoff.k(weights_gain_ratio, 30)
most_important_attributes

#source("DataVisualisation.r")
#scatter(training_data[,c("newsletter","reorder")])

formula_with_most_important_attributes <- as.simple.formula(most_important_attributes, "reorder")
#formula_with_most_important_attributes
