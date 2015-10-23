
# Calcul de la validation croisee
nobs = nrow(X)
mse <- 0

actual    = array(NA, nobs)
predicted = array(NA, nobs)

pb <- txtProgressBar(min=0, max=nobs, style = 3)
setTxtProgressBar(pb, 0)

for (i in 1:nobs){
  Xtemp <- X[-i,]
  Ytemp <- Y[-i,]
  modeltemp <- trainModel(Xtemp,Ytemp)
  
  actual[i]    <- Y[i,]
  predicted[i] <- predictModel(X[i,],modeltemp)
  mse <- mse + (actual[i] - predicted[i])^2
  
  setTxtProgressBar(pb, i)
}
mse <- mse / nobs
rmse <- sqrt(mse)

plot(actual, predicted, main=modelName)

cat("\nValidation croisee: ", rmse, "\n")

