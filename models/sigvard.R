
library(neuralnet)

modelName <- "Sigvard"

# Sigvard est un gentil modele suedois a base de splines.

trainModel <- function(X_, Y_)
{
  pca=prcomp(X_, scale. = TRUE, center=TRUE)
  Xtemp <- as.data.frame(sweep(pca$x,2,pca$sdev,"/"))
  nv=last(which((cumsum(pca$sdev)/sum(pca$sdev))<0.9))
  Xtemp=Xtemp[,1:4]
  
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
  

  mod <- loess(f, span=2, degree=1, data=data)
  
  return(list(mod, a, b, pca, nv))
}



predictModel <- function(x_, model)
{
  mod   = model[[1]]
  a     = model[[2]]
  b     = model[[3]]
  pca   = model[[4]]
  nv    = model[[5]]
  
  xtemp <- as.data.frame(sweep(scale(t(x_), pca$center, pca$scale) %*% pca$rotation,2,pca$sdev,"/"))
  xtemp=xtemp[,1:4]
  
  res=predict(mod, xtemp)
  res<-res*a+b
  
  return(res)
}



