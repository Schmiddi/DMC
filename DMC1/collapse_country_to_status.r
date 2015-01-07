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