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

# The caret package is used (http://topepo.github.io/caret/index.html)
#install.packages("caret")
library(caret)
#install.packages("FSelector")
library(FSelector)

remove_leading_whitespace_factor <- function(X) {
  
  # returns string w/o leading or trailing whitespace
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  
  #------ Workclass ------#
  # Remove leading whitepace from factor a = levels(test_data$education)
  a = levels(X)
  for( i in 1:length(a)){
    levels(X)[levels(X)==a[i]] <- trim(a[i])
  }
  
  return (X)
}

# This function groups countries together according to their income (developing vs. developed)
# Categorization from the world bank was used see: http://data.worldbank.org/about/country-and-lending-groups#High_income
collapse_country_to_status <- function(X) {
  low <- c("Afghanistan"
           ,"Gambia"
           ,"Nepal"
           ,"Bangladesh"  
           ,"Guinea"	
           ,"Niger"
           ,"Benin"	
           ,"Guinea-Bisau"	
           ,"Rwanda"
           ,"Burkina Faso"	
           ,"Haiti"	
           ,"Sierra Leone"
           ,"Burundi"	
           ,"Kenya"	
           ,"Somalia" 
           ,"Cambodia"	
           ,"Korea"	
           ,"Tajikistan"
           ,"Central African Republic"
           ,"Liberia"
           ,"Tanzania"
           ,"Chad"	
           ,"Madagascar"	
           ,"Togo"
           ,"Comoros"	
           ,"Malawi"	
           ,"Uganda"
           ,"Congo"
           ,"Mali"	
           ,"Zimbabwe"
           ,"Eritrea"	
           ,"Mozambique"	 
           ,"Ethiopia"	
           ,"Myanmar")
  
  lower <- c("Armenia"  
             ,"Kiribati"	
             ,"Sao Tome and Principe"
             ,"Bhutan"	
             ,"Kosovo"  	
             ,"Senegal"
             ,"Bolivia"	
             ,"Kyrgyz Republic"	
             ,"Solomon Islands"
             ,"Cameroon"	
             ,"Lao PDR"  
             ,"Laos"
             ,"South Sudan"
             ,"Cabo Verde"	
             ,"Lesotho"	
             ,"Sri Lanka"
             ,"Congo"
             ,"Mauritania"	
             ,"Sudan"
             ,"Côte d'Ivoire"	
             ,"Micronesia"
             ,"Swaziland"
             ,"Djibouti"	
             ,"Moldova"	
             ,"Syrian Arab Republic"
             ,"Egypt"	
             ,"Mongolia"	
             ,"Timor-Leste"
             ,"El Salvador"	
             ,"El-Salvador"
             ,"Morocco"	
             ,"Ukraine"
             ,"Georgia"	
             ,"Nicaragua"	
             ,"Uzbekistan"
             ,"Ghana"	
             ,"Nigeria"  	
             ,"Vanuatu"
             ,"Guatemala"	
             ,"Pakistan"  	
             ,"Vietnam"
             ,"Guyana"	
             ,"Papua New Guinea"  	
             ,"West Bank and Gaza"
             ,"Honduras"	
             ,"Paraguay"	
             ,"Yemen" 
             ,"Indonesia"	
             ,"Philippines"	
             ,"Zambia"
             ,"India"	
             ,"Samoa")
  upper <- c("Angola"  
             ,"Fiji"	
             ,"Palau"
             ,"Albania"	
             ,"Gabon"	
             ,"Panama"
             ,"Algeria"	
             ,"Grenada"	
             ,"Peru"  
             ,"American Samoa"
             ,"Hungary"	
             ,"Romania"
             ,"Argentina"	
             ,"Iran" 	
             ,"Serbia"
             ,"Azerbaijan"	
             ,"Iraq"	
             ,"Seychelles"
             ,'Belarus'	
             ,'Jamaica'	
             ,'South Africa'
             ,'Belize'	
             ,'Jordan'	
             ,'St. Lucia'
             ,'Bosnia and Herzegovina'	
             ,'Kazakhstan'	
             ,'St. Vincent and the Grenadines'
             ,'Botswana'	
             ,'Lebanon'	
             ,'Suriname'
             ,'Brazil'	
             ,'Libya'	
             ,'Thailand'
             ,'Bulgaria'	
             ,'Macedonia' 	
             ,'Tonga'
             ,'China'	
             ,'Malaysia'	
             ,'Tunisia'
             ,'Colombia'	
             ,'Columbia'
             ,'Maldives'	
             ,'Turkey'
             ,'Costa Rica'	
             ,'Marshall Islands'	
             ,'Turkmenistan'
             ,'Cuba'	
             ,'Mauritius'	
             ,'Tuvalu'
             ,'Dominica'	
             ,'Mexico'	
             ,'Venezuela'
             ,'Dominican Republic' 	
             ,'Dominican-Republic'   
             ,'Montenegro'	 
             ,'Ecuador'	
             ,'Namibia'
             ,'Yugoslavia')
  high <- c('Andorra'  
            ,'French Polynesia	'
            ,'Norway'
            ,'Antigua and Barbuda'	
            ,'Germany'
            ,'Oman'
            ,'Aruba'	
            ,'Greece'	
            ,'Poland'
            ,'Australia'	
            ,'Greenland'	
            ,'Portugal'
            ,'Austria'	
            ,'Guam'	
            ,'Puerto Rico'
            ,'Puerto-Rico'
            ,'Bahamas' 
            ,'Hong Kong'
            ,'Hong'
            ,'China'	
            ,'Qatar'
            ,'Bahrain'	
            ,'Iceland'	
            ,'Russian Federation'
            ,'Barbados'	
            ,'Ireland'	
            ,'San Marino'
            ,'Belgium'	
            ,'Isle of Man'	
            ,'Saudi Arabia'
            ,'Bermuda'	
            ,'Israel'	
            ,'Singapore'
            ,'Brunei' 
            ,'Darussalam'	
            ,'Italy'	
            ,'Sint Maarten'
            ,'Canada'	
            ,'Japan'	
            ,'Slovak Republic'
            ,'Cayman Islands'	
            ,'South Korea'	
            ,'Slovenia'
            ,'Channel Islands'
            ,'Kuwait'	
            ,'Spain'
            ,'Chile'	
            ,'Latvia'	
            ,'St. Kitts and Nevis'
            ,'Croatia' 	
            ,'Liechtenstein'	
            ,'St. Martin'
            ,'Curacao'	
            ,'Lithuania'	
            ,'Sweden'
            ,'Cyprus'	
            ,'Luxembourg'	
            ,'Switzerland'
            ,'Czech Republic'
            ,'Macao'
            ,'Taiwan'
            ,'Trinidad and Tobago'
            ,'Trinadad&Tobago'
            ,'Denmark'	
            ,'Malta'	
            ,'Turks and Caicos Islands'
            ,'Estonia'	
            ,'Monaco'	
            ,'United Arab Emirates'
            ,'Equatorial Guinea'	
            ,'Netherlands'	
            ,'United Kingdom'
            ,'Scotland'
            ,'England'
            ,'Faeroe Islands'
            ,'New Caledonia'	
            ,'United-States'
            ,'Finland'	
            ,'New Zealand'	
            ,'Uruguay'
            ,'France'	
            ,'Northern Mariana' 
            ,'Islands'	
            ,'Virgin Islands U.S.'
            ,'Outlying-US(Guam-USVI-etc)')
  
  stati <- c("low"
             ,"lower"
             ,"upper"
             ,"high")
  
  countries <- list(stati, low, lower, upper, high)
  
  # Following outcommented lines are for testing
  #country <- c(" Cuba"," Canada"," South Africa"," China"," Panama"," Germany"," India"," United States"," Israel")
  #dummy <- c("J","J","V","j","V","J","v","V","v")
  #X <- data.frame(country, dummy, stringsAsFactors = FALSE) 
  
  tmp <- data.frame(X["country"], stringsAsFactors = FALSE)
  tmp$country <- as.character(tmp$country)  
  tmp$country <- sub("^\\s+", "", tmp$country)
  names(tmp)[names(tmp)=="country"] <- "status"
  
  for (i in 2:length(countries)) {
    for(j in 1:length(countries[[i]])) {
      tmp$status[tmp$status == countries[[i]][j]] <- countries[[1]][i-1]
    }
  }
  
  X <- cbind(X,tmp)
  X
  return (X)
}

# This function collapses different countries to their geographic region
# edit: use region definition of world bank http://data.worldbank.org/about/country-and-lending-groups#High_income
collapse_country_to_region <- function(X) {
  europe <- c("Albania"
              , "Andorra"
              , "Armenia"
              , "Austria"
              , "Azerbaijan"
              , "Belarus"
              , "Belgium"
              , "Bosnia and Herzegovina"
              , "Bulgaria"
              , "Croatia"
              , "Cyprus"
              , "Czech Republic"
              , "Denmark"
              , "Estonia"
              ,"England"
              , "Faroe Islands"
              , "Finland"
              , "France"
              , "Metropolitan"
              , "Georgia"
              , "Germany"
              , "Gibraltar"
              , "Greece"
              , "Greenland"
              , "Hungary"
              , "Iceland"
              , "Ireland"
              , "Italy"
              , "Kazakhstan"
              , "Kyrgyzstan"
              , "Latvia"
              , "Liechtenstein"
              , "Lithuania"
              , "Luxembourg"
              , "Macedonia"
              , "Malta"
              , "Moldova"
              , "Monaco"
              , "Netherlands"
              , "Norway"
              , "Poland"
              , "Portugal"
              , "Romania"
              , "Russian Federation"
              , "San Marino"
              , "Scotland"
              , "Serbia and Montenegro"
              , "Slovakia"
              , "Slovenia"
              , "Spain"
              , "Svalbard and Jan Mayen Islands"
              , "Sweden"
              , "Switzerland"
              , "Tajikistan"
              , "Turkey"
              , "Turkmenistan"
              , "Ukraine"
              , "United Kingdom"
              , "Uzbekistan"
              , "Vatican"
              , "Yugoslavia")
  africa <- c("Algeria" 
              , "Angola" 
              , "Benin" 
              , "Botswana" 
              , "Bouvet Island" 
              , "Burkina Faso" 
              , "Burundi" 
              , "Cameroon" 
              , "Cape Verde" 
              , "Central African Republic" 
              , "Chad" 
              , "Comoros" 
              , "Congo" 
              , "Congo"
              , "Democratic Republic" 
              , "Cote d’Ivoire" 
              , "Djibouti" 
              , "Egypt" 
              , "Equatorial Guinea"
              , "Eritrea" 
              , "Ethiopia"
              , "Gabon" 
              , "Gambia" 
              , "Ghana" 
              , "Guinea" 
              , "Guinea-Bissau" 
              , "Kenya" 
              , "Lesotho" 
              , "Liberia" 
              , "Libya" 
              , "Madagascar" 
              , "Malawi" 
              , "Mali" 
              , "Mauritania" 
              , "Mauritius"
              , "Mayotte" 
              , "Morocco" 
              , "Mozambique" 
              , "Namibia" 
              , "Niger" 
              , "Nigeria" 
              , "Oman" 
              , "Rwanda" 
              , "Sao Tome and Principe" 
              , "Senegal" 
              , "Seychelles" 
              , "Sierra Leone" 
              , "Somalia"
              , "South Africa" 
              , "Swaziland" 
              , "Tanzania" 
              , "Togo" 
              , "Tunisia" 
              , "Uganda"
              , "Western Sahara" 
              , "Zambia" 
              , "Zimbabwe")
  middleeast <- c("Bahrain" 
                  , "British Indian Ocean Territory" 
                  , "Iran" 
                  , "Iraq" 
                  , "Israel"
                  , "Jordan" 
                  , "Kuwait" 
                  , "Lebanon" 
                  , "Palestinian Territory" 
                  , "Qatar" 
                  , "Reunion"
                  , "Saudi Arabia" 
                  , "United Arab Emirates" 
                  , "Yemen")
  emea <- c(europe,
            africa,
            middleeast)
  
  japac <- c("Australia",
             "Brunei",
             "Cambodia",
             "China",
             "Hong Kong",
             "Hong",
             "Macau",
             "Fiji",
             "Indonesia",
             "Japan",
             "Kiribati",
             "North Korea",
             "South Korea",
             "Laos",
             "Malaysia",
             "Marshall Islands",
             "Federated States of Micronesia",
             "Nauru",
             "New Zealand",
             "Palau",
             "Papua New Guinea",
             "Philippines",
             "Samoa",
             "Singapore",
             "Solomon Islands",
             "Thailand",
             "Timor-Leste",
             "Tonga",
             "Taiwan",
             "Tuvalu",
             "Vanuatu",
             "Vietnam")
  
  saarc <- c("India",
             "Pakistan",
             "Afghanistan",
             "Srilanka",
             "Bhutan",
             "Maldives",
             "Bangladesh",
             "Nepal")
  
  na <- c("Canada",
          "United-States"
          ,"Outlying-US(Guam-USVI-etc)")
  
  lac <- c("Antigua and Barbuda"
           ,"Argentina"
           ,"Barbados"
           ,"Belize"
           ,"Bolivia"
           ,"Brazil"
           ,"CARICOM"
           ,"Chile"
           ,"Colombia"
           ,"Columbia"
           ,"Costa Rica"
           ,"Dominica"
           ,"Dominican-Republic"
           ,"Ecuador"
           ,"El-Salvador"
           ,"Grenada"
           ,"Guatemala"
           ,"Guyana"
           ,"Haiti"
           ,"Honduras"
           ,"Jamaica"
           ,"Mexico"
           ,"Nicaragua"
           ,"Panama"
           ,"Paraguay"
           ,"Peru"
           ,"Puerto-Rico"
           ,"Saint Kitts and Nevis"
           ,"Saint Lucia"
           ,"Saint Vincent and the Grenadines"
           ,"Suriname"
           ,"The Bahamas"
           ,"Trinidad and Tobago"
           ,"Trinadad&Tobago"
           ,"Uruguay"
           ,"Venezuela"
           ,"Cuba")
  
  regions <- c("emea"
               ,"japac"
               ,"saarc"
               ,"na"
               ,"lac")
  
  countries <- list(regions, emea, japac, saarc, na, lac)
  
  # Following outcommented lines are for testing
  #country <- c(" Cuba"," Canada"," South Africa"," China"," Panama"," Germany"," India"," United States"," Israel")
  #dummy <- c("J","J","V","j","V","J","v","V","v")
  #X <- data.frame(country, dummy, stringsAsFactors = FALSE)  
  
  tmp <- data.frame(X["country"], stringsAsFactors = FALSE)
  tmp$country <- as.character(tmp$country)  
  tmp$country <- sub("^\\s+", "", tmp$country)
  names(tmp)[names(tmp)=="country"] <- "region"
  
  for (i in 2:length(countries)) {
    for(j in 1:length(countries[[i]])) {
      tmp$region[tmp$region == countries[[i]][j]] <- countries[[1]][i-1]
    }
  }
  X <- cbind(X,tmp)
  return (X)
}
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
#test_data$edu = NULL 

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

### Review final structure
str(training_data)
str(test_data)

training_data$family_status = NULL
test_data$family_status = NULL

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


###################################
#### 4. Training & Evaluation  ####
###################################
#install.packages("e1071")
library(e1071)


adaBest <- function(attr, X){
  set.seed(42) # do NOT CHANGE this seed
  fitCtrl <- trainControl(method="repeatedcv", number=10, repeats=1)
  mod <- train(attr, data=X, method="AdaBoost.M1", trControl=fitCtrl, metric="Accuracy", preProc = c("center","scale"), tuneGrid=data.frame(coeflearn="Freund",maxdepth=2,mfinal=150))
  return(mod)
}


# Show results and metrics
model = adaBest(formula_with_most_important_attributes, training_data)
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
write.csv(predictions, file="predictions_DropDatabase_1.csv", row.names=FALSE)
