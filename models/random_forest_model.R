# Basic random forest model

cat("Trying random forest model...\n")

modelName <- "Random Forest"


library(randomForest)


trainModel <- function(X_, Y_)
{
  rfmodel = randomForest(X_, Y_)
  return(rfmodel)
}



predictModel <- function(x_, model)
{
  res = as.numeric(predict(model, x_))
  return(res)
}



