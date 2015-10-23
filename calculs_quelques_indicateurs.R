source("init.R")

load(file=paste(ERDFDataFolder,"Mesures",separator,"ConsoMeasParDepart_",zonegeo    ,year, ".xts.RData" ,sep=""))

conso_HTA_xts=ConsoMeasParDepart

nb_HTA=ncol(conso_HTA_xts)

### Calcul de l'indicateur pic journalier moyen par depart HTA et de le max de la conso

if(file.exists("conso_peak_et_pic_moyen.Rdata")) ##Pour ne pas refaire le calcul
{
  load("conso_peak_et_pic_moyen.Rdata")
} else {
  conso_peak     =array(NA,nb_HTA)
  peak_moyen_jour=array(NA,nb_HTA)
  
  pb <- txtProgressBar(min=1, max=nb_HTA, style=3)
  
  for(i in 1:nb_HTA)
  {
    setTxtProgressBar(pb, i)
    conso_peak[i]=max(conso_HTA_xts[,i]) ## Max de la conso
    # moyenne du pic journalier
    peak_moyen_jour[i]=mean(apply.daily(conso_HTA_xts[,i],FUN = max))
  }
  save(conso_peak,peak_moyen_jour,file="conso_peak_et_pic_moyen.Rdata")
}



# Conso moyenne et max entre 17 et 21h et 23 et 7h pour chaque jour
# par depart et ecart journalier
date=time(conso_HTA_xts)
length(date)
date=as.POSIXlt(date)
# date$hour

id_d_y=paste(date$yday,date$year,sep="_")
j=unique(id_d_y) ##indices des jours a parcourir

conso_moy_17_21=array(NA,dim=c(length(j),nb_HTA))
conso_moy_23_7 =array(NA,dim=c(length(j),nb_HTA))
conso_max_17_21=array(NA,dim=c(length(j),nb_HTA))
conso_max_23_7 =array(NA,dim=c(length(j),nb_HTA))
ecart_jour     =array(NA,dim=c(length(j),nb_HTA))


date_conso_j=array(NA,length(j))
id_complet_17_21=NULL
id_complet_23_7=NULL

pb <- txtProgressBar(min=1, max=nb_HTA, style = 3)

for(k in 1:nb_HTA)
{
  setTxtProgressBar(pb, k)
  for(i in 1:length(j))
  {
    #   i=1
    place_=str_locate(string =j[i],pattern = "_")[1]
    id_j=which((as.character(date$yday)==str_sub(j[i],start =1,end =place_-1 ))
               &(as.character(date$year)==str_sub(j[i],start=place_+1,end =place_+3)))
    #   length(id_j)
	ecart_jour[i,k]=max(conso_HTA_xts[id_j,k])-min(conso_HTA_xts[id_j,k])
    ##On a les Id du jour où on est
    id_17_21=which(date[id_j]$hour==17|date[id_j]$hour==18|date[id_j]$hour==19|date[id_j]$hour==20)
    id_complet_17_21=c(id_complet_17_21,id_j[id_17_21])
    conso_moy_17_21[i,k]=sum(conso_HTA_xts[id_j[id_17_21],k])/length(conso_HTA_xts[id_j[id_17_21],k])/2 ##attention ici pas de temps de 30min, à diviser par aure chose si autre pas de temsp
    conso_max_17_21[i,k]=max(conso_HTA_xts[id_j[id_17_21],k])  
    #   
    id_23_7=which(date[id_j]$hour==23|date[id_j]$hour==0|date[id_j]$hour==1|
                    date[id_j]$hour==2|date[id_j]$hour==3|date[id_j]$hour==4|
                    date[id_j]$hour==5|date[id_j]$hour==6)
    conso_moy_23_7[i,k]=sum(conso_HTA_xts[id_j[id_23_7],k])/length(conso_HTA_xts[id_j[id_23_7],k])/2##attention ici pas de temps de 30min, à diviser par aure chose si autre pas de temsp
    
    conso_max_23_7[i,k]=max(conso_HTA_xts[id_j[id_23_7],k])
    
    id_complet_23_7=c(id_complet_23_7,id_j[id_23_7])
    
    date_conso_j[i]=as.character(strptime(paste(as.numeric(str_sub(j[i],start=1       ,end =place_-1)),"-", 
                                                as.numeric(str_sub(j[i],start=place_+1,end =place_+3))+1900,sep=""),format='%j-%Y'))
  }

}
date_conso_j=as.POSIXct(date_conso_j)

