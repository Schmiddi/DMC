scatter <- function(X) {
#  install.packages("AppliedPredictiveModeling")
  library(AppliedPredictiveModeling)
  transparentTheme(trans=.4)
  library(caret)
  featurePlot(x=X[,1:ncol(X)-1]
              ,y=X[ncol(X)]
              ,plot="pairs"
              ,auto.key=list(columns=ncol(X)-1))
}