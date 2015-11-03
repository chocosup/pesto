
start_Date="2011-07-01"
end_Date="2011-08-31"

period=paste0(start_Date,"/",end_Date)

C = ConsoMeasParDepart[period,HTA_names]

tIndices = time(C)

nb_Indices = length(tIndices)
nb_Indices_Day = 24 * 6
nb_Jours   = nb_Indices/nb_Indices_Day



# ---------------------------------------------------------------------------
#                        Define base functions here
# ---------------------------------------------------------------------------
x <- (1:nb_Indices_Day - (nb_Indices_Day+1)/2)

fbase <- function(exposant) {
  y = x^exposant
  y = y - mean(y)
  res = y / sqrt(var(y))
  return(res)
}


fbase0 = matrix(replicate(nb_Jours, x^0     ), 1, nb_Indices)
fbase1 = matrix(replicate(nb_Jours, fbase(1)), 1, nb_Indices)
fbase2 = matrix(replicate(nb_Jours, fbase(2)), 1, nb_Indices)
fbase3 = matrix(replicate(nb_Jours, fbase(3)), 1, nb_Indices)
fbase4 = matrix(replicate(nb_Jours, fbase(4)), 1, nb_Indices)
fbase5 = matrix(replicate(nb_Jours, fbase(5)), 1, nb_Indices)
fbase6 = matrix(replicate(nb_Jours, fbase(6)), 1, nb_Indices)
fbase7 = matrix(replicate(nb_Jours, fbase(7)), 1, nb_Indices)
fbase8 = matrix(replicate(nb_Jours, fbase(8)), 1, nb_Indices)
fbase9 = matrix(replicate(nb_Jours, fbase(9)), 1, nb_Indices)

Std_func = list()
Thermo_func = list()

Std_func[[1]] = rbind(fbase0, fbase1, fbase2, fbase3, fbase4, fbase5, fbase6, fbase7, fbase8, fbase9)
Std_func[[2]] = rbind(fbase0, fbase1, fbase2, fbase3, fbase4, fbase5, fbase6, fbase7, fbase8, fbase9)
Std_func[[3]] = rbind(fbase0, fbase1, fbase2, fbase3, fbase4, fbase5, fbase6, fbase7, fbase8, fbase9)
Std_func[[4]] = rbind(fbase0, fbase1, fbase2, fbase3, fbase4, fbase5, fbase6, fbase7, fbase8, fbase9)

Thermo_func[[1]] = rbind(fbase0, fbase1, fbase2, fbase3, fbase4, fbase5, fbase6, fbase7, fbase8, fbase9)
Thermo_func[[2]] = rbind(fbase0, fbase1, fbase2, fbase3, fbase4, fbase5, fbase6, fbase7, fbase8, fbase9)


# ---------------------------------------------------------------------------
#                            Define coefficients here
# ---------------------------------------------------------------------------

S = souscrit_par_offre[1:length(HTA_names),]

Std_alpha = as.matrix(data.frame(
  residentiel = (S$`bleu domestique`),
  agricole    = (S$`bleu agricole` + S$`jaune agricole`),
  commercants = (S$`bleu artisans et commercants` + S$`jaune commercants`),
  industriel  = (S$Total - S$`bleu domestique` - S$`bleu agricole` - 
                   S$`jaune agricole` - S$`bleu artisans et commercants` - 
                   S$`jaune commercants`)
))

Thermo_alpha = as.matrix( data.frame(
  thermo_residentiel = (S$`bleu domestique`),
  thermo_autres      = (S$Total - S$`bleu domestique`)
))


# ---------------------------------------------------------------------------


nb_Std_Functions = length(Std_func)
nb_Thermo_Functions = length(Thermo_func)
dim_Functions = nrow(Std_func[[1]])

nb_Functions = nb_Std_Functions + nb_Thermo_Functions

nM = nb_Functions * dim_Functions


# Building the raw f matrix
rawf <- matrix(NA,0,nb_Indices)
for (i in 1:nb_Std_Functions) {
  rawf <- rbind(rawf, Std_func[[i]])
}
for (i in 1:nb_Thermo_Functions) {
  rawf <- rbind(rawf, Thermo_func[[i]])
}


# Building the f matrix ("raw f" times temperature when needed)
f <- rawf


Temp = t(as.matrix(rep(temperatures[period],each=3),nb_Indices,1))
Temp = pmin(16-Temp,0) / 4

for (n in 1:nb_Thermo_Functions) {
  i = dim_Functions * (nb_Std_Functions + n - 1)
  for (k in 1:dim_Functions) {
    f[i + k,] = f[i + k,] * Temp
  }
}


alpha = cbind(Std_alpha, Thermo_alpha)




M = matrix(NA,nM,nM)

for(p in 1:nb_Functions) {
  for(l in 1:dim_Functions) {
    ind1 = dim_Functions * (p-1) + l
    for(n in 1:nb_Functions) {
      for(k in 1:dim_Functions) {
        ind2 = dim_Functions * (n-1) + k
        p1 = as.numeric(alpha[,p] %*% alpha[,n])
        p2 = as.numeric(f[ind1,] %*% f[ind2,])
        M[ind1,ind2] = p1  * p2
      }
    }
  }
}


V = matrix(NA,nM,1)

for(p in 1:nb_Functions) {
  for(l in 1:dim_Functions) {
    ind = dim_Functions * (p-1) + l
    
    aux1 = C %*% alpha[,p]
    
    V[ind,1] = matrix(f[ind,],1,nb_Indices) %*% aux1
  }
}


Mp = solve(M)

coeff = Mp %*% V



# profils de charge
rawfp = matrix(NA,nb_Functions,nb_Indices)
fp    = matrix(NA,nb_Functions,nb_Indices)
for (i in 1:nb_Functions)
{
  range = dim_Functions*(i-1) + (1:dim_Functions)
     fp[i,] = as.array(coeff[range] %*%    f[range,])
  rawfp[i,] = as.array(coeff[range] %*% rawf[range,])
}


for (i in 1:nb_Functions) {
  plot(rawfp[i,1:nb_Indices_Day],
       main=colnames(alpha)[i],
       sub=paste("Mean: ", mean(rawfp[i,1:nb_Indices_Day])))
}


# predictions par depart
prediction = alpha %*% fp


pdf(paste0(StatsOutFolder,"Simulation_",year,".pdf"))

day = 1
range = (day-1) * nb_Indices_Day + (1:nb_Indices_Day)

for (i in 1:172) {
  plot(prediction[i,range], main=paste("Depart",i))
  lines(y=as.array(C["2011-07-01",i]),x=range)
}

dev.off()



Cp <- t(as.matrix(C))

cat("Mean consumption: ", mean(Cp)        , "\n")
cat("Mean prediction:  ", mean(prediction), "\n")


depart_Square_Erreur = sqrt(rowMeans( (prediction - Cp)^2 ))
depart_Mean          = rowMeans(Cp)
depart_pc_Erreur     = depart_Square_Erreur / depart_Mean


