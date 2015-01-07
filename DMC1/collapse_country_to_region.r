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