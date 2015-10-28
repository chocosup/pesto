
# Very simple linear model using only a few variables from X

trainModel <- function(X_, Y_)
{
  obj=lars(X_,Y_)
  
  nb_steps=nrow(obj$beta)
  
  coefs = obj$beta[11,]
  aux=obj$normx * coefs
  
  aux=sort(abs(aux),decreasing=TRUE)
  
  lim=as.numeric(aux[10])
  
  columns=names(which(aux >= lim))
  X_ = X_[,columns]
  
  obj2=lars(X_,Y_)
  
  X1 = X_[,columns[1]]
  X2 = X_[,columns[2]]
  X3 = X_[,columns[3]]
  X4 = X_[,columns[4]]
  X5 = X_[,columns[5]]
  X6 = X_[,columns[6]]
  X7 = X_[,columns[7]]
  X8 = X_[,columns[8]]
  X9 = X_[,columns[9]]
  X10= X_[,columns[10]]
  
  fit = lm(Y_ ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10)
  return(list(fit, columns))
}



predictModel <- function(x_, model)
{
  newdata=data.frame(X1 =x_[model[[2]][1]],
                     X2 =x_[model[[2]][2]],
                     X3 =x_[model[[2]][3]],
                     X4 =x_[model[[2]][4]],
                     X5 =x_[model[[2]][5]],
                     X6 =x_[model[[2]][6]],
                     X7 =x_[model[[2]][7]],
                     X8 =x_[model[[2]][8]],
                     X9 =x_[model[[2]][9]],
                     X10=x_[model[[2]][10]])
  
  res = as.numeric(predict.lm(model[[1]],newdata))
  return(res)
}




