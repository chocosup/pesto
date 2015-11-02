SaveFileName  = paste0(StatsOutFolder,"stats_thermo.Rdata")

load(file=MeteoFileName)
temperatures=Temperature[paste(year)]

# Mise a l'echelle des donnees mesurees
indices = seq(1,length(temperatures))
app = approx(indices,temperatures,indices)$y

if (is.na(app[length(app)]))
{
  app[length(app)]=app[length(app)-1]
}
if (is.na(app[1]))
{
  app[1]=app[2]
}

temperatures=xts(app,time(temperatures))
temp_jour=to.daily(apply.daily(temperatures,FUN = mean),drop.time = TRUE)[,1]

if(file.exists(SaveFileName))
{
  cat("Loading thermosensibility stats...\n")
  load(SaveFileName)
  
} else {
  cat("Recomputing thermosensibility stats...\n")
  
  thermo_A      =array(NA,nb_HTA)
  thermo_B      =array(NA,nb_HTA)
  thermo_Erreur =array(NA,nb_HTA)
  thermo_RSquare=array(NA,nb_HTA)
  
  
  pdf(paste0(StatsOutFolder,"Thermo.pdf"))
  for(k in 1:nb_HTA)
  {
    name=names(conso_HTA_xts)[k]
    consos=mean_jour[,k]
    tmp_jour=as.array(temp_jour)
    
    indices=tmp_jour<=16
    plouri=lm(consos[indices]~tmp_jour[indices])
  
    plot(data.frame(x=tmp_jour, y=consos),
         main=paste("Thermo:",name),
         col="cornflowerblue")
    abline(plouri$coefficients, col="chocolate1")
    
    resume=summary(plouri)
    erreur=sqrt(mean(plouri$residuals^2)) / mean(consos[indices]) * 100 # en pourcents
    
    legend("bottomleft", legend =c(paste("R2=",signif(as.numeric(resume[8]),digits=3)),
                                   paste("erreur=",signif(erreur,digits = 3),"%"),
                                   paste("a=",signif(plouri$coefficients[2],digits=3)),
                                   paste("b=",signif(plouri$coefficients[1],digits=3))))
    
    thermo_A[k]=plouri$coefficients[2]
    thermo_B[k]=plouri$coefficients[1]
    thermo_Erreur[k] =erreur
    thermo_RSquare[k]=as.numeric(resume[8])
  }
  dev.off()
  
  save(thermo_A,
       thermo_B,
       thermo_Erreur,
       thermo_RSquare,
       file=SaveFileName)
}
