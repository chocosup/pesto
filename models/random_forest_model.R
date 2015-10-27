# Basic random forest model

library(randomForest)


trainModel <- function(X_, Y_)
{
  Xtmp = X_
  Ytmp = as.matrix(Y_)
  
  Xmins  = apply(Xtmp, 2, min)
  Xscale = apply(Xtmp, 2, max) - Xmins
  Ymins  = apply(Ytmp, 2, min)
  Yscale = apply(Ytmp, 2, max) - Ymins
  
  Xtmp <- scale(Xtmp, center = Xmins, scale = Xscale)
  Ytmp <- scale(Ytmp, center = Ymins, scale = Yscale)
  
  rfmodel = randomForest(Xtmp, Ytmp)
  return(list(rfmodel, Xmins, Xscale, Ymins, Yscale))
}



predictModel <- function(x_, model)
{
  rdfrst = model[[1]]
  Xmins  = model[[2]]
  Xscale = model[[3]]
  Ymins  = model[[4]]
  Yscale = model[[5]]
  
  xtmp = scale(t(x_), center = Xmins, scale = Xscale)
  
  res = as.numeric(predict(rdfrst, xtmp))
  res = res * Yscale + Ymins
  return(res)
}