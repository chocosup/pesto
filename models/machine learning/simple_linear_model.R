
# Very simple linear model using only a few variables from X

trainModel <- function(X_, Y_)
{
  
  r <- Y_
  
  ind1 <- which.max(abs(cor(X_,r)))
  X1 <- X_[,ind1]
  r  <- Y_ - predict(lm(Y_~X1),data.frame(X1=X_[,ind1]))
  
  ind2 <- which.max(abs(cor(X_,r)))
  X2 <- X_[,ind2]
  r  <- Y_ - predict(lm(Y_~X1+X2),data.frame(X1=X_[,ind1],
                                             X2=X_[,ind2]))
  
  ind3 <- which.max(abs(cor(X_,r)))
  X3 <- X_[,ind3]
  r  <- Y_ - predict(lm(Y_~X1+X2+X3),data.frame(X1=X_[,ind1],
                                                X2=X_[,ind2],
                                                X3=X_[,ind3]))
  

#   X1 = X_[,"Conso"]
#   X2 = X_[,"pourc_CONSTRUCTION_PROVISOIRE"]
  # X3 = X_[,"pourc_REFERENT_PERSONNE_AU_FOYER"]
  # X4 = X_[,"pourc_ASCENSEUR"]
  # X5 = X_[,"pourc_BATIMENT_2LOGEMENTS_ET_PLUS"]
  
  # X5 = X_[,"jaune agricole"]
  # X6 = X_[,"jaune bureaux-enseignement-sante"]
  # X7 = X_[,"jaune commercants"]
  # X8 = X_[,"jaune industrie"]
  
  fit = lm(Y_ ~ X1 + X2 + X3) # + X4 + X5)
  
  return( list(fit, ind1, ind2, ind3) )
}



predictModel <- function(x_, model)
{
  newdata=data.frame(X1=x_[model[[2]] ],
                     X2=x_[model[[3]] ],
                     X3=x_[model[[4]] ])
#                      X4=x_["pourc_ASCENSEUR"]
#                      X5=x_["pourc_BATIMENT_2LOGEMENTS_ET_PLUS"])
  
  res = as.numeric(predict.lm(model[[1]],newdata))
  return(res)
}



