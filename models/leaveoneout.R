
if (sampleSize >= nobs || sampleSize <= 0) {
  indices=1:nobs
  sampleSize=nobs
} else {
  if (shuffleSample) {
    indices=shuffle(nobs)[1:sampleSize]
  } else {
    indices=1:sampleSize
  }
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


plot(actual, predicted, asp=1,
     main=modelName,
     sub=paste("Mean: ", mean(Y), "    -    RMSE: ", rmse))
abline(0,1)

cat("\nValidation croisee: ", rmse, "\n")

