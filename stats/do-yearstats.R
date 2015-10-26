SaveFileName  = paste0(StatsOutFolder,"stats_conso.Rdata")

if(file.exists(SaveFileName))
{
  cat("Loading all yearly stats...\n")
  load(SaveFileName)
  
} else {
  cat("Recomputing all yearly stats...\n")
  cat("Starting: ", format(Sys.time()),"\n")
  
  unlink(StatsOutFolder, recursive=TRUE, force=TRUE)
  dir.create(StatsOutFolder)
  
  cat("Computing year indicators...\n")
  pb <- txtProgressBar(min=1, max=nb_HTA, style=3)
  
  date=time(conso_HTA_xts)
  length(date)
  date=as.POSIXlt(date)
  
  id_d_y=paste(date$yday,date$year,sep="_")
  j=unique(id_d_y)  # Indices des jours a parcourir
  
  nb_Jours = length(j)
  
  dates_j =as.POSIXct(seq.Date(
    as.Date(paste0(year,"-01-01")),
    as.Date(paste0(year,"-12-31")),"days"))
  
  moy_annuel      =array(NA,nb_HTA)
  var_annuel      =array(NA,nb_HTA)
  conso_peak_an   =array(NA,nb_HTA)
  conso_peak_date =array(NA,nb_HTA)
  
  peak_jour_moyen =array(NA,nb_HTA)
  nadir_jour_moyen=array(NA,nb_HTA)
  delta_jour_moyen=array(NA,nb_HTA)
  delta_mean_peak =array(NA,nb_HTA)
  var_jour_moyen  =array(NA,nb_HTA)
  
  mean_jour      =xts(array(NA,dim=c(nb_Jours,nb_HTA)),dates_j)
  var_jour       =xts(array(NA,dim=c(nb_Jours,nb_HTA)),dates_j)
  peak_jour      =xts(array(NA,dim=c(nb_Jours,nb_HTA)),dates_j)
  nadir_jour     =xts(array(NA,dim=c(nb_Jours,nb_HTA)),dates_j)
  ecart_jour     =xts(array(NA,dim=c(nb_Jours,nb_HTA)),dates_j)
  ecart_maxmean_jour=xts(array(NA,dim=c(nb_Jours,nb_HTA)),dates_j)
  
  conso_moy_17_21=xts(array(NA,dim=c(nb_Jours,nb_HTA)),dates_j)
  conso_moy_23_7 =xts(array(NA,dim=c(nb_Jours,nb_HTA)),dates_j)
  conso_max_17_21=xts(array(NA,dim=c(nb_Jours,nb_HTA)),dates_j)
  conso_max_23_7 =xts(array(NA,dim=c(nb_Jours,nb_HTA)),dates_j)
  
  for(k in 1:nb_HTA)
  {
    setTxtProgressBar(pb, k)
    
    moy_annuel[k]     =mean(conso_HTA_xts[,k])
    conso_peak_an[k]  =max(conso_HTA_xts[,k])
    var_annuel[k]     =var(conso_HTA_xts[,k])
    conso_peak_date[k]=as.POSIXct(date[which.max(conso_HTA_xts[,k])],
                                  origin="1970-01-01")  # Date du max annuel
    
    mean_jour[,k] =to.daily(apply.daily(conso_HTA_xts[,k],FUN = mean))[,1]
    var_jour[,k]  =to.daily(apply.daily(conso_HTA_xts[,k],FUN = var ))[,1]
    peak_jour[,k] =to.daily(apply.daily(conso_HTA_xts[,k],FUN = max ))[,1]
    nadir_jour[,k]=to.daily(apply.daily(conso_HTA_xts[,k],FUN = min ))[,1]
    
    ecart_jour[,k]        =peak_jour[,k] - nadir_jour[,k]
    ecart_maxmean_jour[,k]=peak_jour[,k] -  mean_jour[,k]
    
    peak_jour_moyen[k] =mean(peak_jour[,k])  # Moyenne du pic journalier
    nadir_jour_moyen[k]=mean(nadir_jour[,k]) # Moyenne du pic journalier
    delta_jour_moyen[k]=mean(ecart_jour[,k])
    delta_mean_peak[k] =mean(ecart_maxmean_jour[,k])
    var_jour_moyen[k]  =mean(var_jour[,k])
  }
  
  
  id_complet_17_21=NULL
  id_complet_23_7=NULL
  
  # Conso moyenne et max entre 17 et 21h et 23 et 7h pour chaque jour
  # par depart et ecart journalier
  
  cat("\nComputing day-to-day indicators...\n")
  pb2 <- txtProgressBar(min=1, max=nb_Jours, style=3)
  
  for(i in 1:nb_Jours)
  {
    setTxtProgressBar(pb2, i)
    
    # Dates d'aujourd'hui
    curdate=as.character(dates_j[i])
    curdate17_21=  paste0(curdate," 17:00:00/",curdate," 20:59:59")
    curdate23_07=c(paste0(curdate," 00:00:00/",curdate," 06:59:59"),
                   paste0(curdate," 23:00:00/",curdate," 23:59:59"))
    
    for(k in 1:nb_HTA)
    {
      # De 17h a 21h
      conso_17_21=conso_HTA_xts[curdate17_21,k]
      conso_moy_17_21[i,k]=mean(conso_17_21)
      conso_max_17_21[i,k]=max(conso_17_21)
      
      # De 23h a 07h
      conso_23_7=conso_HTA_xts[curdate23_07,k]
      conso_moy_23_7[i,k]=mean(conso_23_7)
      conso_max_23_7[i,k]=max(conso_23_7)
    }
  }
  
  
  save(
    moy_annuel,
    var_annuel,
    
    conso_peak_an,
    conso_peak_date,
    
    peak_jour_moyen,
    nadir_jour_moyen,
    delta_jour_moyen,
    delta_mean_peak,
    var_jour_moyen,
    
    mean_jour,
    var_jour,
    peak_jour,
    nadir_jour,
    ecart_jour,
    ecart_maxmean_jour,
    
    conso_moy_17_21,
    conso_moy_23_7,
    conso_max_17_21,
    conso_max_23_7,
    
    Stats_conso,
    dates_j,
    file=SaveFileName)
  
  cat("\nDone: ", format(Sys.time()),"\n")
}




