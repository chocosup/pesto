
# Very simple linear model using only a few variables from X

cat("Trying simple linear model...\n")


trainModel <- function(X_, Y_)
{
  X1 = X_[,"mean_NB_PERSONNES_SCOLARISEES"]
  X2 = X_[,"pourc_CHAUFFAGE_CENTRAL_INDIV"]
  X3 = X_[,"nb_IRIS"]
  X4 = X_[,"pourc_RESIDENCE_PRINCIPALE"]
  X5 = X_[,"NON RENSEIGNE"]
  
#  X5 = X_[,"jaune agricole"]
#  X6 = X_[,"jaune bureaux-enseignement-sante"]
#  X7 = X_[,"jaune commercants"]
#  X8 = X_[,"jaune industrie"]
  
  fit = lm(Y_ ~ X1 + X2 + X3 + X4 + X5)
  return(fit)
}



predictModel <- function(x_, model)
{
  newdata=data.frame(X1=x_["pourc_CHAMBRE_HOTEL"],
                     X2=x_["bleu agricole"],
                     X3=x_["bleu domestique"],
                     X4=x_["bleu equipement collectif"],
                     X5=x_["jaune agricole"],
                     X6=x_["jaune bureaux-enseignement-sante"],
                     X7=x_["jaune commercants"],
                     X8=x_["jaune industrie"],
                     X9=x_["NON RENSEIGNE"])
  
  res = as.numeric(predict.lm(model,newdata))
  return(res)
}
