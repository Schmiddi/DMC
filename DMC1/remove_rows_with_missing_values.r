remove_rows_with_missing_values <- function(X) {
  X<-X[rowSums(X == '?') == 0,]
  X<-X[rowSums(X == ' ?') == 0,]
  X<-X[rowSums(X == '? ') == 0,]
  X<-X[rowSums(X == ' ? ') == 0,]
}