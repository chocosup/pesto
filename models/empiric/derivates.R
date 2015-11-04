Derivconso = as.matrix(NewConsoMeas)[-1,] - as.matrix(NewConsoMeas)[-length(NewConsoMeas[,1]),]

dsdevs = c()
for (i in 1:ncol(Derivconso)) { dsdevs = append(dsdevs, sd(Derivconso[,i])) }

NewDerivConso = scale(Derivconso, center = FALSE, scale = dsdevs)


tmp = pmin(10, NewDerivConso)
NewDerivConso = matrix(tmp,dim(NewDerivConso)[1],dim(NewDerivConso)[2])

tmp = pmax(-10, NewDerivConso)
NewDerivConso = matrix(tmp,dim(NewDerivConso)[1],dim(NewDerivConso)[2])

rm(tmp)

integrer = function(M)
{
  I = M
  for(i in 1:ncol(M))
  {
    I[,i] = cumsum(M[,i])
  }
  return(I)
}