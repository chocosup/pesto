cat("[TERTIAIRE] Starting.\n")

#start_Date="2011-01-01"
#end_Date="2011-12-31"

start_Date="2011-01-01"
end_Date="2011-04-31"

# start_Date="2011-01-01"
# end_Date="2011-03-31"


period=paste0(start_Date,"/",end_Date)

Cg = ConsoMeasParDepart[period,HTA_names]

tIndices = time(Cg)

nb_Indices = length(tIndices)
nb_Indices_Day = 24 * 6
nb_Jours   = nb_Indices/nb_Indices_Day

do_leaveoneout = FALSE
leaveoneout_HTA = c(1,18)


if (!do_leaveoneout) {
  leaveoneout_HTA = 0
} else {
  prediction = matrix(NA, length(leaveoneout_HTA), nb_Indices)
}

# ---------------------------------------------------------------------------
#                        Define base functions here
# ---------------------------------------------------------------------------
Std_func = list()
Thermo_func = list()


x  <- (1:nb_Indices_Day - (nb_Indices_Day+1)/2)
x_ <- (2*pi)*(0:(nb_Indices_Day-1))/(nb_Indices_Day-1)

fbase <- function(exposant) {
  y = x^exposant
  y = y - mean(y)
  res = y / sqrt(var(y))
  return(res)
}

fcosharmonique <- function(frequency) {
  y = cos(frequency*x_)
  return(y)
}

fsinharmonique <- function(frequency) {
  y = sin(frequency*x_)
  return(y)
}

# fbase0 = matrix(replicate(nb_Jours, x^0     ), 1, nb_Indices)
# fbase1 = matrix(replicate(nb_Jours, fbase(1)), 1, nb_Indices)
# fbase2 = matrix(replicate(nb_Jours, fbase(2)), 1, nb_Indices)
# fbase3 = matrix(replicate(nb_Jours, fbase(3)), 1, nb_Indices)
# fbase4 = matrix(replicate(nb_Jours, fbase(4)), 1, nb_Indices)
# fbase5 = matrix(replicate(nb_Jours, fbase(5)), 1, nb_Indices)
# fbase6 = matrix(replicate(nb_Jours, fbase(6)), 1, nb_Indices)
# fbase7 = matrix(replicate(nb_Jours, fbase(7)), 1, nb_Indices)
# fbase8 = matrix(replicate(nb_Jours, fbase(8)), 1, nb_Indices)
# fbase9 = matrix(replicate(nb_Jours, fbase(9)), 1, nb_Indices)
# 
# Std_func[[1]] = rbind(fbase0, fbase1, fbase2, fbase3, fbase4, fbase5, fbase6, fbase7, fbase8, fbase9)
# Std_func[[2]] = rbind(fbase0, fbase1, fbase2, fbase3, fbase4, fbase5, fbase6, fbase7, fbase8, fbase9)
# Std_func[[3]] = rbind(fbase0, fbase1, fbase2, fbase3, fbase4, fbase5, fbase6, fbase7, fbase8, fbase9)
# Std_func[[4]] = rbind(fbase0, fbase1, fbase2, fbase3, fbase4, fbase5, fbase6, fbase7, fbase8, fbase9)
# Thermo_func[[1]] = rbind(fbase0, fbase1, fbase2, fbase3, fbase4, fbase5, fbase6, fbase7, fbase8, fbase9)
# Thermo_func[[2]] = rbind(fbase0, fbase1, fbase2, fbase3, fbase4, fbase5, fbase6, fbase7, fbase8, fbase9)



fbase0     = matrix(replicate(nb_Jours, x^0              ) , 1, nb_Indices)
fbasecos1  = matrix(replicate(nb_Jours, fcosharmonique(1)) , 1, nb_Indices)
fbasesin1  = matrix(replicate(nb_Jours, fsinharmonique(1)) , 1, nb_Indices)
fbasecos2  = matrix(replicate(nb_Jours, fcosharmonique(2)) , 1, nb_Indices)
fbasesin2  = matrix(replicate(nb_Jours, fsinharmonique(2)) , 1, nb_Indices)
fbasecos3  = matrix(replicate(nb_Jours, fcosharmonique(3)) , 1, nb_Indices)
fbasesin3  = matrix(replicate(nb_Jours, fsinharmonique(3)) , 1, nb_Indices)
fbasecos4  = matrix(replicate(nb_Jours, fcosharmonique(4)) , 1, nb_Indices)
fbasesin4  = matrix(replicate(nb_Jours, fsinharmonique(4)) , 1, nb_Indices)
fbasecos5  = matrix(replicate(nb_Jours, fcosharmonique(5)) , 1, nb_Indices)
fbasesin5  = matrix(replicate(nb_Jours, fsinharmonique(5)) , 1, nb_Indices)
fbasecos6  = matrix(replicate(nb_Jours, fcosharmonique(6)) , 1, nb_Indices)
fbasesin6  = matrix(replicate(nb_Jours, fsinharmonique(6)) , 1, nb_Indices)
fbasecos7  = matrix(replicate(nb_Jours, fcosharmonique(7)) , 1, nb_Indices)
fbasesin7  = matrix(replicate(nb_Jours, fsinharmonique(7)) , 1, nb_Indices)
fbasecos8  = matrix(replicate(nb_Jours, fcosharmonique(8)) , 1, nb_Indices)
fbasesin8  = matrix(replicate(nb_Jours, fsinharmonique(8)) , 1, nb_Indices)
fbasecos9  = matrix(replicate(nb_Jours, fcosharmonique(9)) , 1, nb_Indices)
fbasesin9  = matrix(replicate(nb_Jours, fsinharmonique(9)) , 1, nb_Indices)
fbasecos10 = matrix(replicate(nb_Jours, fcosharmonique(10)), 1, nb_Indices)
fbasesin10 = matrix(replicate(nb_Jours, fsinharmonique(10)), 1, nb_Indices)
fbasecos11 = matrix(replicate(nb_Jours, fcosharmonique(11)), 1, nb_Indices)
fbasesin11 = matrix(replicate(nb_Jours, fsinharmonique(11)), 1, nb_Indices)
fbasecos12 = matrix(replicate(nb_Jours, fcosharmonique(12)), 1, nb_Indices)
fbasesin12 = matrix(replicate(nb_Jours, fsinharmonique(12)), 1, nb_Indices)
fbasecos13 = matrix(replicate(nb_Jours, fcosharmonique(13)), 1, nb_Indices)
fbasesin13 = matrix(replicate(nb_Jours, fsinharmonique(13)), 1, nb_Indices)
fbasecos14 = matrix(replicate(nb_Jours, fcosharmonique(14)), 1, nb_Indices)
fbasesin14 = matrix(replicate(nb_Jours, fsinharmonique(14)), 1, nb_Indices)
fbasecos15 = matrix(replicate(nb_Jours, fcosharmonique(15)), 1, nb_Indices)
fbasesin15 = matrix(replicate(nb_Jours, fsinharmonique(15)), 1, nb_Indices)
fbasecos16 = matrix(replicate(nb_Jours, fcosharmonique(16)), 1, nb_Indices)
fbasesin16 = matrix(replicate(nb_Jours, fsinharmonique(16)), 1, nb_Indices)
fbasecos17 = matrix(replicate(nb_Jours, fcosharmonique(17)), 1, nb_Indices)
fbasesin17 = matrix(replicate(nb_Jours, fsinharmonique(17)), 1, nb_Indices)
fbasecos18 = matrix(replicate(nb_Jours, fcosharmonique(18)), 1, nb_Indices)
fbasesin18 = matrix(replicate(nb_Jours, fsinharmonique(18)), 1, nb_Indices)
fbasecos19 = matrix(replicate(nb_Jours, fcosharmonique(19)), 1, nb_Indices)
fbasesin19 = matrix(replicate(nb_Jours, fsinharmonique(19)), 1, nb_Indices)
fbasecos20 = matrix(replicate(nb_Jours, fcosharmonique(20)), 1, nb_Indices)
fbasesin20 = matrix(replicate(nb_Jours, fsinharmonique(20)), 1, nb_Indices)


Std_func[[1]] = rbind(fbase0, fbasecos1, fbasesin1, fbasecos2, fbasesin2, fbasecos3, fbasesin3, fbasecos4, fbasesin4, fbasecos5, fbasesin5, fbasecos6, fbasesin6, fbasecos7, fbasesin7, fbasecos8, fbasesin8, fbasecos9, fbasesin9, fbasecos10, fbasesin10, fbasecos11, fbasesin11, fbasecos12, fbasesin12, fbasecos13, fbasesin13, fbasecos14, fbasesin14, fbasecos15, fbasesin15, fbasecos16, fbasesin16, fbasecos17, fbasesin17, fbasecos18, fbasesin18, fbasecos19, fbasesin19, fbasecos20, fbasesin20)
Std_func[[2]] = rbind(fbase0, fbasecos1, fbasesin1, fbasecos2, fbasesin2, fbasecos3, fbasesin3, fbasecos4, fbasesin4, fbasecos5, fbasesin5, fbasecos6, fbasesin6, fbasecos7, fbasesin7, fbasecos8, fbasesin8, fbasecos9, fbasesin9, fbasecos10, fbasesin10, fbasecos11, fbasesin11, fbasecos12, fbasesin12, fbasecos13, fbasesin13, fbasecos14, fbasesin14, fbasecos15, fbasesin15, fbasecos16, fbasesin16, fbasecos17, fbasesin17, fbasecos18, fbasesin18, fbasecos19, fbasesin19, fbasecos20, fbasesin20)
Std_func[[3]] = rbind(fbase0, fbasecos1, fbasesin1, fbasecos2, fbasesin2, fbasecos3, fbasesin3, fbasecos4, fbasesin4, fbasecos5, fbasesin5, fbasecos6, fbasesin6, fbasecos7, fbasesin7, fbasecos8, fbasesin8, fbasecos9, fbasesin9, fbasecos10, fbasesin10, fbasecos11, fbasesin11, fbasecos12, fbasesin12, fbasecos13, fbasesin13, fbasecos14, fbasesin14, fbasecos15, fbasesin15, fbasecos16, fbasesin16, fbasecos17, fbasesin17, fbasecos18, fbasesin18, fbasecos19, fbasesin19, fbasecos20, fbasesin20)
Std_func[[4]] = rbind(fbase0, fbasecos1, fbasesin1, fbasecos2, fbasesin2, fbasecos3, fbasesin3, fbasecos4, fbasesin4, fbasecos5, fbasesin5, fbasecos6, fbasesin6, fbasecos7, fbasesin7, fbasecos8, fbasesin8, fbasecos9, fbasesin9, fbasecos10, fbasesin10, fbasecos11, fbasesin11, fbasecos12, fbasesin12, fbasecos13, fbasesin13, fbasecos14, fbasesin14, fbasecos15, fbasesin15, fbasecos16, fbasesin16, fbasecos17, fbasesin17, fbasecos18, fbasesin18, fbasecos19, fbasesin19, fbasecos20, fbasesin20)
Thermo_func[[1]] = rbind(fbase0, fbasecos1, fbasesin1, fbasecos2, fbasesin2, fbasecos3, fbasesin3, fbasecos4, fbasesin4, fbasecos5, fbasesin5, fbasecos6, fbasesin6, fbasecos7, fbasesin7, fbasecos8, fbasesin8, fbasecos9, fbasesin9, fbasecos10, fbasesin10, fbasecos11, fbasesin11, fbasecos12, fbasesin12, fbasecos13, fbasesin13, fbasecos14, fbasesin14, fbasecos15, fbasesin15, fbasecos16, fbasesin16, fbasecos17, fbasesin17, fbasecos18, fbasesin18, fbasecos19, fbasesin19, fbasecos20, fbasesin20)
Thermo_func[[2]] = rbind(fbase0, fbasecos1, fbasesin1, fbasecos2, fbasesin2, fbasecos3, fbasesin3, fbasecos4, fbasesin4, fbasecos5, fbasesin5, fbasecos6, fbasesin6, fbasecos7, fbasesin7, fbasecos8, fbasesin8, fbasecos9, fbasesin9, fbasecos10, fbasesin10, fbasecos11, fbasesin11, fbasecos12, fbasesin12, fbasecos13, fbasesin13, fbasecos14, fbasesin14, fbasecos15, fbasesin15, fbasecos16, fbasesin16, fbasecos17, fbasesin17, fbasecos18, fbasesin18, fbasecos19, fbasesin19, fbasecos20, fbasesin20)


# ---------------------------------------------------------------------------
#                            Define coefficients here
# ---------------------------------------------------------------------------

S = souscrit_par_offre[1:length(HTA_names),]

# S = conso_par_offre[1:length(HTA_names),] * replicate()



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


alphaG = cbind(Std_alpha, Thermo_alpha) / 1000

for (iter in 1:length(leaveoneout_HTA)) {
  
  pbIter <- txtProgressBar(min=0, max=length(leaveoneout_HTA), style=3)
  setTxtProgressBar(pbIter, 0)
  
  if (do_leaveoneout) {
    alpha = alphaG[-leaveoneout_HTA[iter],]
    C= Cg[,-leaveoneout_HTA[iter]]
  } else {
    alpha = alphaG
    C = Cg
  }
  
  cat("[TERTIAIRE] Computing matrix coefficient.\n")
  M = matrix(NA,nM,nM)
  
  pb <- txtProgressBar(min=1, max=nb_Functions*nb_Functions*dim_Functions*dim_Functions, style=3)
  
  for(p in 1:nb_Functions) {
    for(l in 1:dim_Functions) {
      ind1 = dim_Functions * (p-1) + l
      for(n in 1:nb_Functions) {
        for(k in 1:dim_Functions) {
          
          setTxtProgressBar(pb, ind2 + nb_Functions * dim_Functions * ind1)
          
          ind2 = dim_Functions * (n-1) + k
          p1 = as.numeric(alpha[,p] %*% alpha[,n])
          p2 = as.numeric(f[ind1,] %*% f[ind2,])
          M[ind1,ind2] = p1  * p2
        }
      }
    }
  }
  
  
  cat("[TERTIAIRE] Computing vector coefficient.\n")
  
  V = matrix(NA,nM,1)
  
  for(p in 1:nb_Functions) {
    for(l in 1:dim_Functions) {
      ind = dim_Functions * (p-1) + l
      
      aux1 = C %*% alpha[,p]
      
      V[ind,1] = matrix(f[ind,],1,nb_Indices) %*% aux1
    }
  }
  
  
  cat("[TERTIAIRE] Solving system.\n")
  
  coeff = solve(M,V)
  
  
  # profils de charge
  rawfp = matrix(NA,nb_Functions,nb_Indices)
  fp    = matrix(NA,nb_Functions,nb_Indices)
  for (i in 1:nb_Functions)
  {
    range = dim_Functions*(i-1) + (1:dim_Functions)
    fp[i,] = as.array(coeff[range] %*%    f[range,])
    rawfp[i,] = as.array(coeff[range] %*% rawf[range,])
  }
  
  cat("[TERTIAIRE] Plotting and saving.\n")
  
  if (!do_leaveoneout) {
    openPDF(paste0(StatsOutFolder,"BaseFunctions_",year))
    
    range <- 1:nb_Indices_Day
    for (i in 1:nb_Functions) {
      par(lab=c(24,5,5))
      plot(y=rawfp[i,range],
           x=(range/6),
           type="l",
           main=colnames(alpha)[i],
           sub=paste("Mean: ", mean(rawfp[i,1:nb_Indices_Day])))
    }
    
    closePDF()
  }
  
  # predictions par depart
  if (do_leaveoneout) {
    prediction[iter,] <- alpha[leaveoneout_HTA[iter],] %*% fp
  }else {
    prediction = alpha %*% fp
  }
  
  
  nb_jours_sample = 7
  nb_sample = 10
  for (i in 1:nb_sample) {
    day = floor((i * (nb_Jours-nb_jours_sample)) / (nb_sample + 1))
    str_date = as.character(as.Date(start_Date) + day)
    
    
    openPDF(paste0(StatsOutFolder,"Simulation_",str_date))
    
    day = 1
    range = (day-1) * nb_Indices_Day + (1:(nb_jours_sample*nb_Indices_Day))
    
    if (do_leaveoneout) {
      for (i in 1:length(leaveoneout_HTA)) {
        par(lab=c(24,5,5))
        dat <- cbind( as.matrix(prediction[i,range]),
                      as.matrix(Cg[range,leaveoneout_HTA[i]]) )
        matplot(y=dat,x=(range/6), type = c("l"),pch=1,col = 1:2,main=paste("Depart",leaveoneout_HTA[i]))
      }
    } else {
      for (i in 1:length(HTA_names)) {
        par(lab=c(24,5,5))
        dat <- cbind( as.matrix(prediction[i,range]),
                      as.matrix(C[range,i]) )
        matplot(y=dat,x=(range/6), type = c("l"),pch=1,col = 1:2,main=paste("Depart",i))
      }
    }
    
    
    closePDF()
  }
  if (length(leaveoneout_HTA)>1) {
    setTxtProgressBar(pbIter, iter)
  }
}


Cp <- t(as.matrix(C))

if (!do_leaveoneout) {
  depart_Square_Erreur = sqrt(rowMeans( (prediction - Cp)^2 ))
  depart_Mean          = rowMeans( abs(Cp) )
  depart_pc_Erreur     = depart_Square_Erreur / depart_Mean
  depart_Sylvain_Erreur = rowMeans( abs(prediction - Cp) / prediction )
  
  plot(sort(depart_pc_Erreur), type="h")
  plot(sort(depart_Sylvain_Erreur), type="h")
  plot(depart_pc_Erreur, depart_Sylvain_Erreur)
  
  
  cat(" Start date:",start_Date,"\n",
      "End   date:",end_Date,"\n",
      "RMSE    error: ", mean(depart_pc_Erreur), "\n",
      "Sylvain error: ", mean(depart_Sylvain_Erreur), "\n",
      file=paste0(StatsOutFolder,"log_",year,".txt"))
}



