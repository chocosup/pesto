
start_Date="2011-07-01"
end_Date="2011-08-31"

period=paste0(start_Date,"/",end_Date)

C = ConsoMeasParDepart[period,HTA_names]

tIndices = time(C)

nb_Indices = length(tIndices)
nb_Jours   = nb_Indices/24/6


x <- (1:(24*6) - 72.5) / 36

fbase0 = matrix(replicate(nb_Jours, x^0), 1, nb_Indices)
fbase1 = matrix(replicate(nb_Jours, x^1), 1, nb_Indices)
fbase2 = matrix(replicate(nb_Jours, x^2), 1, nb_Indices)
fbase3 = matrix(replicate(nb_Jours, x^3), 1, nb_Indices)
fbase4 = matrix(replicate(nb_Jours, x^4), 1, nb_Indices)
fbase5 = matrix(replicate(nb_Jours, x^5), 1, nb_Indices)
fbase6 = matrix(replicate(nb_Jours, x^6), 1, nb_Indices)
fbase7 = matrix(replicate(nb_Jours, x^7), 1, nb_Indices)
fbase8 = matrix(replicate(nb_Jours, x^8), 1, nb_Indices)
fbase9 = matrix(replicate(nb_Jours, x^9), 1, nb_Indices)

Temp = t(as.matrix(rep(temperatures[period],each=3),nb_Indices,1))


f = rbind(fbase0, fbase1, fbase2, fbase3, fbase4, fbase5, fbase6, fbase7, fbase8, fbase9,
          fbase0, fbase1, fbase2, fbase3, fbase4, fbase5, fbase6, fbase7, fbase8, fbase9,
          fbase0, fbase1, fbase2, fbase3, fbase4, fbase5, fbase6, fbase7, fbase8, fbase9,
          fbase0, fbase1, fbase2, fbase3, fbase4, fbase5, fbase6, fbase7, fbase8, fbase9,
          fbase0, fbase1, fbase2, fbase3, fbase4, fbase5, fbase6, fbase7, fbase8, fbase9,
          fbase0, fbase1, fbase2, fbase3, fbase4, fbase5, fbase6, fbase7, fbase8, fbase9)

for (n in 5:6) {
  for (k in 1:10) {
    f[10*(n-1) + k,] = f[10*(n-1) + k,] * Temp
  }
}

nM = 4*10 + 2*10


S = souscrit_par_offre[1:length(HTA_names),]

alpha = as.matrix(
  data.frame(
    residentiel = (S$`bleu domestique`)                                      / 1000,
    agricole    = (S$`bleu agricole` + S$`jaune agricole`)                   / 1000,
    commercants = (S$`bleu artisans et commercants` + S$`jaune commercants`) / 1000,
    industriel  = (S$Total - S$`bleu domestique` - S$`bleu agricole` - 
                   S$`jaune agricole` - S$`bleu artisans et commercants` - 
                   S$`jaune commercants`)                                    / 1000,
    thermo_residentiel = (S$`bleu domestique`)           / 1000,
    thermo_autres      = (S$Total - S$`bleu domestique`) / 1000
  )
)





M = matrix(NA,nM,nM)

for(p in 1:6) {
  for(l in 1:10) {
    ind1 = 10 * (p-1) + l
    for(n in 1:6) {
      for(k in 1:10) {
        ind2 = 10 * (n-1) + k
        
        p1 = as.numeric(alpha[,c(1)] %*% alpha[,c(2)])
        
        p2 = as.numeric(f[ind1,] %*% f[ind2,])
        
        M[ind1,ind2] = p1  * p2
      }
    }
  }
}

Mp = M^-1

V = matrix(NA,nM,1)

for(p in 1:6) {
  for(l in 1:10) {
    ind = 10 * (p-1) + l
    
    aux1 = C %*% alpha[,1]
    
    V[ind,1] = matrix(f[ind,],1,nb_Indices) %*% aux1
  }
}


coeff = Mp * V




