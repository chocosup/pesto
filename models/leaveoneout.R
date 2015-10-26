
# Calcul de la validation croisee
mse <- 0

actual    = array(NA, nobs)
predicted = array(NA, nobs)


if (sampleSize >= nobs || sampleSize <= 0) {
  indices=1:nobs
} else {
  indices=shuffle(nobs)[1:sampleSize]
}


pb <- txtProgressBar(min=0, max=length(indices), style = 3)
setTxtProgressBar(pb, 0)


for (k in 1:length(indices)){
  i=indices[k]
  Xtemp <- X[-i,]
  Ytemp <- Y[-i,]
  modeltemp <- trainModel(Xtemp,Ytemp)
  
  actual[i]    <- Y[i,]
  predicted[i] <- predictModel(X[i,],modeltemp)
  mse <- mse + (actual[i] - predicted[i])^2
  
  setTxtProgressBar(pb, k)
}
mse <- mse / length(indices)
rmse <- sqrt(mse)


plot(actual, predicted, main=modelName)

cat("\nValidation croisee: ", rmse, "\n")

