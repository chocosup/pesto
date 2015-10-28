
if (sampleSize >= nobs || sampleSize <= 0) {
  indices=1:nobs
  sampleSize=nobs
} else {
  # indices=shuffle(nobs)[1:sampleSize]
  indices=1:sampleSize
}

actual    = array(NA, sampleSize)
predicted = array(NA, sampleSize)


pb <- txtProgressBar(min=0, max=sampleSize, style = 3)
setTxtProgressBar(pb, 0)


# Calcul de la validation croisee
mse <- 0

for (k in 1:sampleSize){
  i=indices[k]
  Xtemp <- X[-i,]
  Ytemp <- Y[-i,]
  modeltemp <- trainModel(Xtemp,Ytemp)
  
  actual[i]    <- Y[i,]
  predicted[i] <- predictModel(X[i,],modeltemp)
  mse <- mse + (actual[i] - predicted[i])^2
  
  setTxtProgressBar(pb, k)
}
mse <- mse / sampleSize
rmse <- sqrt(mse)


plot(actual, predicted, main=modelName)

cat("\nValidation croisee: ", rmse, "\n")

