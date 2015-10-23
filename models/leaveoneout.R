
# Calcul de la validation croisee
nobs = nrow(X)
mse <- 0

pb <- txtProgressBar(min=0, max=nobs, style = 3)
setTxtProgressBar(pb, 0)

for (i in 1:nobs){
  Xtemp <- X[-i,]
  Ytemp <- Y[-i,]
  modeltemp <- trainModel(Xtemp,Ytemp)
  
  actual    <- Y[i,]
  predicted <- predictModel(X[i,],modeltemp)
  mse <- mse + (actual - predicted)^2
  
  setTxtProgressBar(pb, i)
}
mse <- mse / nobs
rmse <- sqrt(mse)

cat("Validation croisee: ", rmse, "\n")

