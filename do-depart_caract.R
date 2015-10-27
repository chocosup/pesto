source("init.R")

load(file=paste(ERDFDataFolder,"Customers_"                             ,zonegeo,"_",year,"_PESTO.Rdata",sep=""))
load(file=paste(ERDFDataFolder,"Mesures",separator,"ConsoMeasParDepart_",zonegeo    ,year, ".xts.RData" ,sep=""))
load(file=paste(ERDFDataFolder,"Mesures",separator,"ReorgaParDepart_"   ,zonegeo    ,year, ".xts.RData" ,sep=""))


Customers=client_pesto_final
# Correspondance liste de client/depart HTA

#######nb clients BT et HTA
## fichier ERDF qui verifie qu'on a bien tous les clients
FileName=paste(ERDFDataFolder,"denombrement_HTA_146_191.csv",sep="")
tmpdata=read.csv2(FileName,blank.lines.skip = TRUE)
names(tmpdata)

Subsetyear=as.numeric(str_sub(as.character(tmpdata$Mois.d.observation),1,4))==year

tmpdata=tmpdata[Subsetyear,]
tmpdata=tmpdata[!is.na(tmpdata$Mois.d.observation),]

tmpdata$Nombre.de.Producteurs.HTA[is.na(tmpdata$Nombre.de.Producteurs.HTA)]=0

unique_HTA=(unique(Customers$CODE_GDO))
unique_HTA=sort(unique_HTA)

nb_HTA = length(unique_HTA)

nb_clients_BT_per_depart    =array(NA,nb_HTA)
nb_client_BT                =array(NA,nb_HTA)
nb_client_HTA               =array(NA,nb_HTA)
nb_prod_HTA                 =array(NA,nb_HTA)
conso_totale_BT             =array(NA,nb_HTA)
part_conso_BT_residentiel_BT=array(NA,nb_HTA)
conso_totale_HTA            =array(NA,nb_HTA)
pourcentage_reorga          =array(NA,nb_HTA)
nb_pics                     =array(NA,nb_HTA)
PS                          =array(NA,nb_HTA)

pb <- txtProgressBar(min=1, max=nb_HTA, style = 3)

indices_depart=seq(along=ReorgaParDepart$LOSCOC0005)
indice_customer=seq(along=Customers$CODE_GDO)

length(indice_customer)
indices_same_HTA=list()
communes        =list()
nb_commune    =array(NA,nb_HTA)
Communes_depart=array(NA,dim=c(nb_HTA,20))

for(i in 1:nb_HTA)
{
  HTA=unique_HTA[i]
# i=1
  PS[i]=str_sub(HTA,1,5)
  setTxtProgressBar(pb, i)
  indices_same_HTA_1=indice_customer[Customers$CODE_GDO==HTA]
  indices_same_HTA_1=indices_same_HTA_1[!is.na(indices_same_HTA_1)]
  indices_same_HTA[[i]]=indices_same_HTA_1
  #Customers$CODE_GDO[indices_same_HTA_1]
  
  communes[[i]]=unique(Customers$CODE_INSEE[indices_same_HTA_1])
  communes[[i]]=communes[[i]][!is.na(communes[[i]])]
  for(i88 in 1:length( communes[[i]]))
  {
    Communes_depart[i,i88]=communes[[i]][i88]
  }
  
  nb_commune[i]=length(communes[[i]])
  
  nb_clients_BT_per_depart[i]=length(indices_same_HTA[[i]])
  
  
  indice_HTA_denomb=seq(along=tmpdata$Code.GDO.Depart.HTA)
  indice_HTA_denomb=indice_HTA_denomb[as.character(tmpdata$Code.GDO.Depart.HTA)==as.character(unique_HTA[i])]
  
  nb_client_BT[i] =tmpdata$Nombre.de.clients.BT.du.poste.HTA[indice_HTA_denomb]
  nb_client_HTA[i]=tmpdata$Nombre.de.clients.HTA.du.poste.HTA[indice_HTA_denomb]
  nb_prod_HTA[i]  =tmpdata$Nombre.de.Producteurs.HTA[indice_HTA_denomb]
  
  #ratio conso tertiaire/residentiel BT
  
  indices=(Customers$CODE_ACTIVITE[indices_same_HTA_1]==0)&((str_sub(Customers$NOM_CLIENT[indices_same_HTA_1],1,2)=="MM")|(str_sub(Customers$NOM_CLIENT[indices_same_HTA_1],1,2)=="MR"))
  indices=(Customers$CODE_ACTIVITE[indices_same_HTA_1]==0)
  indice_residential=indices_same_HTA_1[indices]
  # Customers[indice_residential,c(1,11,12)]
  
  conso_totale_BT[i]=sum(Customers$Conso_customer_Wh[indices_same_HTA_1])
  part_conso_BT_residentiel_BT[i]=sum(Customers$Conso_customer_Wh[indice_residential])/(conso_totale_BT[i])*100
  if (PS[i]!="BRERC" & PS[i]!="2OUES")
# if (PS[i]!="2OUES")
  {
    consoMea=ConsoMeasParDepart[,match(c(paste(HTA)), names(ConsoMeasParDepart))]
    
    conso_totale_HTA[i]=sum(consoMea)/6
    indices_reorga    =indices[consoMea==0]
    indices_pas_reorga=indices[consoMea!=0]

    pourcentage_reorga[i]=length(indices_reorga)/length(indices_depart)*100
    if(pourcentage_reorga[i]!=100)
    {
      cdc=consoMea[indices_pas_reorga]
#     nb_pics[i]=length(indices[abs(diff(cdc))>1000])
    }
  }
}

conso_totale_BT_kWh=conso_totale_BT/1000

difference_nb_client=nb_clients_BT_per_depart-nb_client_BT
relative_diff_nb_BT=(difference_nb_client)/(nb_client_BT)*100

# plot(nb_clients_BT_per_depart)
# plot(nb_poste_DP)
# plot(relative_diff_nb_BT)
# indices=seq(along=relative_diff_nb_BT)
# indices[relative_diff_nb_BT>0.5]



Recap_HTA=data.frame(
  PS=PS,
  Depart_HTA=unique_HTA,
  nb_clients_BT_per_depart=nb_clients_BT_per_depart,
  nb_client_BT_csv=nb_client_BT,
  conso_par_client=conso_totale_HTA/nb_client_BT,
  ecart_moy_conso_par_client=conso_totale_HTA/nb_client_BT/(mean(conso_totale_HTA/nb_client_BT)),
  relative_diff_nb_BT=relative_diff_nb_BT,
  nb_client_HTA=nb_client_HTA,
  nb_prod_HTA=nb_prod_HTA,
  part_conso_BT_residentiel_BT=part_conso_BT_residentiel_BT,
  nb_commune=nb_commune,
  conso_totale_BT_kWh=conso_totale_BT_kWh,
  conso_totale_HTA_brute=conso_totale_HTA,
  relative_diff_conso=(conso_totale_HTA-conso_totale_BT_kWh)/conso_totale_HTA*100,
  pourcentage_reorga=pourcentage_reorga,
# nb_pics_sans_reorga=nb_pics,
  Communes=Communes_depart
)


write.csv2(Recap_HTA        ,file=paste(OuputFolder,"Recap_HTA_",year,".csv",sep=""))
write.csv2(Customers[1:100,],file=paste(OuputFolder,"en_tete"   ,year,".csv",sep=""))


hist(Recap_HTA$part_conso_BT_residentiel_BT[!is.na(Recap_HTA$part_conso_BT_residentiel_BT)],
     main="Part de consommation residentielle dans la consommation des clients BT",
     xlab="Pourcentage",ylab="Occurence")
mean(Recap_HTA$part_conso_BT_residentiel_BT,na.rm=T)


indices_pas_rearga=which(ReorgaParDepart$ABERSC0001==1)

plot(ConsoMeasParDepart$ABERSC0001[indices_pas_rearga])

View(Recap_HTA)
