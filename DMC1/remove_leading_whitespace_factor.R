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