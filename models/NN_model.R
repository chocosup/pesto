
# Very simple linear model using only a few variables from X

cat("Trying NN model...\n")

modelName <- "NN model"


library(neuralnet)

trainModel <- function(X_, Y_)
{
  Xtemp <- X
  Ytemp <- Y
  colnames(Ytemp)<-c("Y")
  
  data=cbind(Xtemp, Ytemp)
  n=colnames(data)
  for (i in 1:length(n)) {
    n[i]<-gsub('-','_',n[i])
    n[i]<-gsub(' ','_',n[i])
  }
  colnames(data)<-n
  
  maxs <- apply(data, 2, max) 
  mins <- apply(data, 2, min)
  data <- as.data.frame(scale(data, center = mins, scale = maxs - mins))
  
  maxs <- apply(Xtemp, 2, max) 
  mins <- apply(Xtemp, 2, min)
  
  a=(max(Ytemp)-min(Ytemp))
  b=min(Ytemp)
  
  f <- as.formula(paste('Y ~', paste(n[!n %in% 'Y'], collapse = ' + ')))
  
  mynet <- neuralnet(f, data, hidden = c(30,25,20,15,10,5,1), threshold = 0.01)
  
  return(list(mynet, mins, maxs, a, b))
}



predictModel <- function(x_, model)
{
  mynet = model[[1]]
  mins  = model[[2]]
  maxs  = model[[3]]
  a     = model[[4]]
  b     = model[[5]]
  
  xtemp <- as.data.frame(scale(t(x_), center = mins, scale = maxs - mins))
  
  res=compute(mynet, xtemp)
  res<-res$net.result
  res<-res*a+b
  
  return(res)
}



