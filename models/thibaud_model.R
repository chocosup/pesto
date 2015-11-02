

trainModel <- function(X_, Y_)
{
  X1 = X_[,"Clients"] * X_[,"mean_NB_PERSONNES"] * X_[,"bleu domestique"]
  X2 = X_[,"pourc_CHAUFFAGE_ELECTRIQUE"]*X_[,"Clients"] * X_[,"mean_NB_PERSONNES"] * X_[,"bleu domestique"]
  
  
  return(lm(Y_ ~ X1 + X2))
}

predictModel <- function(x_, model)
{
  x1 = x_["Clients"] * x_["mean_NB_PERSONNES"] * x_["bleu domestique"]
  x2 = x_["pourc_CHAUFFAGE_ELECTRIQUE"] * x_["Clients"] * x_["mean_NB_PERSONNES"] * x_["bleu domestique"]
  res = predict(model, data.frame(X1=x1,X2=x2))
  return(as.numeric(res))
}


