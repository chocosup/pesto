
library(neuralnet)

modelName <- "NN model"

trainModel <- function(X_, Y_)
{
  Xtemp <- X_
  Ytemp <- as.data.frame(Y_)
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
  
  # mynet <- neuralnet(f, data, hidden = c(30,25,20,15,10,5,1), threshold = 0.01)
  
  mynet <- neuralnet(f, data, hidden = c(30,25,20,15,10,5,1), threshold = 0.001,
                     stepmax = 2e+04, rep = 1, startweights = NULL,
                     learningrate.limit = NULL,
                     learningrate=0.001, lifesign="none",
                     lifesign.step = 1000,
                     err.fct = "sse",
                     linear.output = FALSE,
                     constant.weights = NULL, likelihood = TRUE)
  
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



