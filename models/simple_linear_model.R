
# Very simple linear model using only a few variables from X

cat("Trying simple linear model...\n")

modelName <- "Simple linear model"

trainModel <- function(X_, Y_)
{
  X1 = X_[,"bleu domestique"]
  X2 = X_[,"bleu artisans et commercants"]
  #   X3 = X_[,"pourc_REFERENT_PERSONNE_AU_FOYER"]
  #   X4 = X_[,"pourc_ASCENSEUR"]
  #   X5 = X_[,"pourc_BATIMENT_2LOGEMENTS_ET_PLUS"]
  
  #  X5 = X_[,"jaune agricole"]
  #  X6 = X_[,"jaune bureaux-enseignement-sante"]
  #  X7 = X_[,"jaune commercants"]
  #  X8 = X_[,"jaune industrie"]
  
  fit = lm(Y_ ~ X1 + X2)# + X3 + X4 + X5)
  return(fit)
}



predictModel <- function(x_, model)
{
  newdata=data.frame(X1=x_["bleu domestique"],
                     X2=x_["bleu artisans et commercants"])#,
  #                      X3=x_["pourc_REFERENT_PERSONNE_AU_FOYER"],
  #                      X4=x_["pourc_ASCENSEUR"],
  #                      X5=x_["pourc_BATIMENT_2LOGEMENTS_ET_PLUS"])
  
  res = as.numeric(predict.lm(model,newdata))
  return(res)
}
