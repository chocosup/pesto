
library(neuralnet)

modelName <- "Gunnar"

trainModel <- function(X_, Y_)
{
  pca=prcomp(X_, scale. = TRUE, center=TRUE)
  Xtemp <- as.data.frame(sweep(pca$x,2,pca$sdev,"/"))
  nv=last(which((cumsum(pca$sdev)/sum(pca$sdev))<0.95))
  Xtemp=Xtemp[,1:nv]
  
  Ytemp <- as.data.frame(Y_)
  colnames(Ytemp)<-c("Y")
  a=(max(Ytemp)-min(Ytemp))
  b=min(Ytemp)
  Ytemp <- (Ytemp-b)/a
  
  data=cbind(Xtemp, Ytemp)
  n=colnames(data)
  for (i in 1:length(n)) {
    n[i]<-gsub('-','_',n[i])
    n[i]<-gsub(' ','_',n[i])
  }
  colnames(data)<-n
  
  f <- as.formula(paste('Y ~', paste(n[!n %in% 'Y'], collapse = ' + ')))
  
  # mynet <- neuralnet(f, data, hidden = c(30,25,20,15,10,5,1), threshold = 0.01, algorithm='backprop', learningrate=0.01, err.fct = "sse", act.fct="tanh", rep=5, linear.output = FALSE)
  #   mynet <- neuralnet(f, data, hidden = c(20,20,10,5,3), threshold = 0.01,
  #     stepmax = 2e+04, rep = 3, startweights = NULL,
  #     learningrate.limit = NULL,
  #     learningrate.factor = list(minus = 0.5, plus = 1.2),
  #     learningrate=0.01, lifesign = "none",
  #     lifesign.step = 1000, algorithm = "backprop",
  #     err.fct = "sse", act.fct = "logistic",
  #     linear.output = FALSE,
  #     constant.weights = NULL, likelihood = FALSE)
  
  mynet <- neuralnet(f, data, hidden = c(30,25,20,15,10,5,1), threshold = 0.001,
                     stepmax = 2e+04, rep = 1, startweights = NULL,
                     learningrate.limit = NULL,
                     learningrate=0.001, lifesign="none",
                     lifesign.step = 1000,
                     err.fct = "sse",
                     linear.output = FALSE,
                     constant.weights = NULL, likelihood = TRUE)
  
  return(list(mynet, a, b, pca, nv))
}



predictModel <- function(x_, model)
{
  mynet = model[[1]]
  a     = model[[2]]
  b     = model[[3]]
  pca   = model[[4]]
  nv    = model[[5]]
  
  xtemp <- as.data.frame(sweep(scale(t(x_), pca$center, pca$scale) %*% pca$rotation,2,pca$sdev,"/"))
  xtemp=xtemp[,1:nv]
  
  res=compute(mynet, xtemp)
  res<-res$net.result
  res<-res*a+b
  
  return(res)
}



