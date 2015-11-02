trainModel <- function(X_, Y_)
{
  return(mean(Y_))
}



predictModel <- function(x_, model)
{
  return(as.numeric(model))
}



