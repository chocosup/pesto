
trainModel <- function(X_, Y_)
{
  obj<- lars(X_,Y_)
  r  <- cv.lars(X_,Y_,plot.it=FALSE, K=20,type='lasso')
  bestfraction <- r$index[which.min(r$cv)]
  
  return(list(obj, bestfraction))
}



predictModel <- function(x_, model)
{
  obj <- model[[1]]
  bestfraction <- model[[2]]
  
  res <- predict(obj,t(x_),s=bestfraction,type="fit",mode="fraction")$fit
  
  return( as.numeric(res) )
}



