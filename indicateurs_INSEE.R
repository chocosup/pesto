load(file=paste(INSEEDataFolder,"Dep69_2012.RData"          ,sep=""))

Recensement=logement_69
remove(logement_69)
Customers=client_pesto_final
remove(client_pesto_final)

Recensement <- Recensement[which(Recensement$COMMUNE %in% Customers$CODE_INSEE), ]
Recensement <- Recensement[which(!is.na(Recensement$COMMUNE)), ]
Recensement[,"IRIS_completed"]=array(NA,length(Recensement$IRIS))
Poids=as.character(Recensement$IPONDL)
Poids=gsub(",",".",Poids)
Recensement$IPONDL=as.numeric(Poids)
Recensement$AGEMEN8 = as.character(Recensement$AGEMEN8)
Recensement$AGEMEN8 = as.numeric(Recensement$AGEMEN8)
Recensement$AGEMEN8[which(Recensement$AGEMEN8==15)] <- 17
Recensement$AGEMEN8[which(Recensement$AGEMEN8==20)] <- 22
Recensement$AGEMEN8[which(Recensement$AGEMEN8==25)] <- 32
Recensement$AGEMEN8[which(Recensement$AGEMEN8==40)] <- 47
Recensement$AGEMEN8[which(Recensement$AGEMEN8==55)] <- 60
Recensement$AGEMEN8[which(Recensement$AGEMEN8==65)] <- 72
Recensement$AGEMEN8[which(Recensement$AGEMEN8==80)] <- 85
Recensement$INEEM = as.character(Recensement$INEEM)
Recensement$INEEM = as.numeric(Recensement$INEEM)
Recensement$INP11M = as.character(Recensement$INP11M)
Recensement$INP11M = as.numeric(Recensement$INP11M)
Recensement$INP16M = as.character(Recensement$INP16M)
Recensement$INP16M = as.numeric(Recensement$INP16M)
Recensement$INP18M = as.character(Recensement$INP18M)
Recensement$INP18M = as.numeric(Recensement$INP18M)
Recensement$INP19M = as.character(Recensement$INP19M)
Recensement$INP19M = as.numeric(Recensement$INP19M)
Recensement$INP24M = as.character(Recensement$INP24M)
Recensement$INP24M = as.numeric(Recensement$INP24M)
Recensement$INP3M = as.character(Recensement$INP3M)
Recensement$INP3M = as.numeric(Recensement$INP3M)
Recensement$INP60M = as.character(Recensement$INP60M)
Recensement$INP60M = as.numeric(Recensement$INP60M)
Recensement$INP65M = as.character(Recensement$INP65M)
Recensement$INP65M = as.numeric(Recensement$INP65M)
Recensement$INP6M = as.character(Recensement$INP6M)
Recensement$INP6M = as.numeric(Recensement$INP6M)
Recensement$INP75M = as.character(Recensement$INP75M)
Recensement$INP75M = as.numeric(Recensement$INP75M)
Recensement$INPAM = as.character(Recensement$INPAM)
Recensement$INPAM = as.numeric(Recensement$INPAM)
Recensement$INPER = as.character(Recensement$INPER)
Recensement$INPER = as.numeric(Recensement$INPER)
Recensement$INPER1 = as.character(Recensement$INPER1)
Recensement$INPER1 = as.numeric(Recensement$INPER1)
Recensement$INPER2 = as.character(Recensement$INPER2)
Recensement$INPER2 = as.numeric(Recensement$INPER2)
Recensement$INPOM = as.character(Recensement$INPOM)
Recensement$INPOM = as.numeric(Recensement$INPOM)
Recensement$INPSM = as.character(Recensement$INPSM)
Recensement$INPSM = as.numeric(Recensement$INPSM)
Recensement$NBPI = as.character(Recensement$NBPI)
Recensement$NBPI = as.numeric(Recensement$NBPI)

for (iris in unique(Customers$IRIS)) {
  Recensement$IRIS_completed[which(Recensement$IRIS==iris)] = iris
  if (!is.na(iris) && (as.numeric(as.character(iris)) %% 10000)==0) {
    Recensement$IRIS_completed[which(Recensement$COMMUNE==as.numeric(as.character(iris))/10000)] = iris
  }
}

nb_HTA = length(HTA_names)
nb_PS = length(PS_names)

code_HTA = array(NA,nb_HTA+nb_PS)
poste_source=array(NA,nb_HTA+nb_PS)
nb_IRIS=array(NA,nb_HTA+nb_PS)
pourc_HLM=array(0,nb_HTA+nb_PS)
pourc_IMMIGRATION=array(0,nb_HTA+nb_PS)
mean_ANNEE_EMMENAGEMENT=array(0,nb_HTA+nb_PS)
pourc_ASCENSEUR=array(0,nb_HTA+nb_PS)
pourc_RESIDENCE_PRINCIPALE=array(0,nb_HTA+nb_PS)
pourc_LOGEMENT_OCCASIONNEL=array(0,nb_HTA+nb_PS)
pourc_RESIDENCE_SECONDAIRE=array(0,nb_HTA+nb_PS)
mean_AGE=array(0,nb_HTA+nb_PS)
pourc_CHAUFFAGE_CENTRAL_COLLECTIF=array(0,nb_HTA+nb_PS)
pourc_CHAUFFAGE_CENTRAL_INDIV=array(0,nb_HTA+nb_PS)
pourc_CHAUFFAGE_ELECTRIQUE=array(0,nb_HTA+nb_PS)
pourc_COMBUSTIBLE_ELECTRICITE=array(0,nb_HTA+nb_PS)
pourc_STATIONNEMENT=array(0,nb_HTA+nb_PS)
mean_NB_ELEVES_ETUDIANTS_14ANSPLUS=array(0,nb_HTA+nb_PS)
mean_NB_11ANSMOINS=array(0,nb_HTA+nb_PS)
mean_NB_16ANSMOINS=array(0,nb_HTA+nb_PS)
mean_NB_18ANSMOINS=array(0,nb_HTA+nb_PS)
mean_NB_19ANSMOINS=array(0,nb_HTA+nb_PS)
mean_NB_24ANSMOINS=array(0,nb_HTA+nb_PS)
mean_NB_3ANSMOINS=array(0,nb_HTA+nb_PS)
mean_NB_60ANSPLUS=array(0,nb_HTA+nb_PS)
mean_NB_65ANSPLUS=array(0,nb_HTA+nb_PS)
mean_NB_6ANSMOINS=array(0,nb_HTA+nb_PS)
mean_NB_75ANSPLUS=array(0,nb_HTA+nb_PS)
mean_NB_PERSONNES_ACTIVES=array(0,nb_HTA+nb_PS)
mean_NB_PERSONNES=array(0,nb_HTA+nb_PS)
mean_NB_PERSONNES_MASCULIN=array(0,nb_HTA+nb_PS)
mean_NB_PERSONNES_FEMININ=array(0,nb_HTA+nb_PS)
mean_NB_PERSONNES_ACTIVES_AVEC_EMPLOI=array(0,nb_HTA+nb_PS)
mean_NB_PERSONNES_SCOLARISEES=array(0,nb_HTA+nb_PS)
mean_NB_PIECES_LOGEMENT=array(0,nb_HTA+nb_PS)
pourc_PROPRIETAIRE_LOGEMENT=array(0,nb_HTA+nb_PS)
pourc_LOCATAIRE_LGMNT_VIDE_NON_HLM=array(0,nb_HTA+nb_PS)
pourc_LOCATAIRE_LGMNT_VIDE_HLM=array(0,nb_HTA+nb_PS)
pourc_LOCATAIRE_MEUBLE=array(0,nb_HTA+nb_PS)
pourc_LOGE_GRATUITEMENT=array(0,nb_HTA+nb_PS)
pourc_LOGMNT_MOINS_40M2=array(0,nb_HTA+nb_PS)
pourc_LOGMNT_40M2_A_100M2=array(0,nb_HTA+nb_PS)
pourc_LOGMNT_PLUS_100M2=array(0,nb_HTA+nb_PS)
pourc_REFERENT_ACTIFS=array(0,nb_HTA+nb_PS)
pourc_REFERENT_CHOMEUR=array(0,nb_HTA+nb_PS)
pourc_REFERENT_RETRAITE=array(0,nb_HTA+nb_PS)
pourc_REFERENT_ETUDIANT=array(0,nb_HTA+nb_PS)
pourc_REFERENT_MOINS_14ANS=array(0,nb_HTA+nb_PS)
pourc_REFERENT_PERSONNE_AU_FOYER=array(0,nb_HTA+nb_PS)
pourc_REFERENT_AUTRES_INACTIFS=array(0,nb_HTA+nb_PS)
pourc_LOGEMENT_ISOLE=array(0,nb_HTA+nb_PS)
pourc_LOGEMENT_JUMELE=array(0,nb_HTA+nb_PS)
pourc_BATIMENT_2LOGEMENTS_ET_PLUS=array(0,nb_HTA+nb_PS)
pourc_BATIMENT_NON_HABITATION=array(0,nb_HTA+nb_PS)
pourc_CONSTRUCTION_PROVISOIRE=array(0,nb_HTA+nb_PS)
pourc_MAISONS=array(0,nb_HTA+nb_PS)
pourc_APPARTEMENTS=array(0,nb_HTA+nb_PS)
pourc_LOGEMENT_FOYER=array(0,nb_HTA+nb_PS)
pourc_CHAMBRE_HOTEL=array(0,nb_HTA+nb_PS)
pourc_HABITATION_DE_FORTUNE=array(0,nb_HTA+nb_PS)
pourc_PIECE_INDEPENDANTE=array(0,nb_HTA+nb_PS)

pb <- txtProgressBar(min=0, max=nb_HTA, style = 3)
setTxtProgressBar(pb, 0)

for(i in 1:nb_HTA) {
  divs=array(0,33)
  hta_cur=HTA_names[i]
  irisS=unique(Customers$IRIS[which(Customers$CODE_GDO==hta_cur)])
  irisS=irisS[which(!is.na(irisS))]
  poste_source[i]=substr(hta_cur,1,5)
  irisS = irisS[!is.na(irisS)]
  irisS = irisS[which(irisS %in% Recensement$IRIS_completed)]
  nb_IRIS[i]=length(irisS)
  weightIris=0
  if(length(irisS)!=0) {
    for(j in 1:length(irisS)) {
      weightIris=length(which(Customers$CODE_GDO==hta_cur & Customers$IRIS==irisS[j]))/length(which(Customers$IRIS==irisS[j]))
      indices=(Recensement$IRIS_completed==irisS[j])
      pourc_HLM[i]=pourc_HLM[i]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$HLML==1))])
      divs[1]=divs[1]+weightIris*sum(Recensement$IPONDL[which(indices & (Recensement$HLML==1 | Recensement$HLML==2))])
      pourc_IMMIGRATION[i]=pourc_IMMIGRATION[i]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$IMMIM==1))])
      divs[2]=divs[2]+weightIris*sum(Recensement$IPONDL[which(indices & (Recensement$IMMIM==1 | Recensement$IMMIM==2))])
      mean_ANNEE_EMMENAGEMENT[i]=mean_ANNEE_EMMENAGEMENT[i]+weightIris*sum(Recensement$IPONDL[which(indices & (Recensement$AEMM!=0))]*Recensement$AEMM[which(indices & (Recensement$AEMM!=0))])
      divs[3]=divs[3]+weightIris*sum(Recensement$IPONDL[which(indices & (Recensement$AEMM!=0))])
      pourc_ASCENSEUR[i]=pourc_ASCENSEUR[i]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$ASCEN==1))])
      divs[4]=divs[4]+weightIris*sum(Recensement$IPONDL[which(indices & (Recensement$ASCEN==1 | Recensement$ASCEN==2))])
      pourc_RESIDENCE_PRINCIPALE[i]=pourc_RESIDENCE_PRINCIPALE[i]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$CATL==1))])
      pourc_LOGEMENT_OCCASIONNEL[i]=pourc_LOGEMENT_OCCASIONNEL[i]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$CATL==2))])
      pourc_RESIDENCE_SECONDAIRE[i]=pourc_RESIDENCE_SECONDAIRE[i]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$CATL==3))])
      divs[5]=divs[5]+weightIris*sum(Recensement$IPONDL[which(indices & (Recensement$CATL==1 | Recensement$CATL==2 | Recensement$CATL==3 | Recensement$CATL==4))])
      mean_AGE[i]=mean_AGE[i]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$AGEMEN8))]*Recensement$AGEMEN8[which(indices & !is.na(Recensement$AGEMEN8))])
      divs[6]=divs[6]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$AGEMEN8))])
      pourc_CHAUFFAGE_CENTRAL_COLLECTIF[i]=pourc_CHAUFFAGE_CENTRAL_COLLECTIF[i]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$CHFL==1))])
      pourc_CHAUFFAGE_CENTRAL_INDIV[i]=pourc_CHAUFFAGE_CENTRAL_INDIV[i]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$CHFL==2))])
      pourc_CHAUFFAGE_ELECTRIQUE[i]=pourc_CHAUFFAGE_ELECTRIQUE[i]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$CHFL==3))])
      divs[7]=divs[7]+weightIris*sum(Recensement$IPONDL[which(indices & (Recensement$CHFL==1 | Recensement$CHFL==2 | Recensement$CHFL==3 | Recensement$CHFL==4))])
      pourc_COMBUSTIBLE_ELECTRICITE[i]=pourc_COMBUSTIBLE_ELECTRICITE[i]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$CMBL==4))])
      divs[8]=divs[8]+weightIris*sum(Recensement$IPONDL[which(indices & (Recensement$CMBL==1 | Recensement$CMBL==2 | Recensement$CMBL==3 | Recensement$CMBL==4 | Recensement$CMBL==5 | Recensement$CMBL==6))])
      pourc_STATIONNEMENT[i]=pourc_STATIONNEMENT[i]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$GARL==1))])
      divs[9]=divs[9]+weightIris*sum(Recensement$IPONDL[which(indices & (Recensement$GARL==1 | Recensement$GARL==2))])
      mean_NB_ELEVES_ETUDIANTS_14ANSPLUS[i]=mean_NB_ELEVES_ETUDIANTS_14ANSPLUS[i]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INEEM))]*Recensement$INEEM[which(indices & !is.na(Recensement$INEEM))])
      divs[10]=divs[10]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INEEM))])
      mean_NB_11ANSMOINS[i]=mean_NB_11ANSMOINS[i]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INP11M))]*Recensement$INP11M[which(indices & !is.na(Recensement$INP11M))])
      divs[11]=divs[11]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INP11M))])
      mean_NB_16ANSMOINS[i]=mean_NB_16ANSMOINS[i]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INP16M))]*Recensement$INP16M[which(indices & !is.na(Recensement$INP16M))])
      divs[12]=divs[12]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INP16M))])
      mean_NB_18ANSMOINS[i]=mean_NB_18ANSMOINS[i]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INP18M))]*Recensement$INP18M[which(indices & !is.na(Recensement$INP18M))])
      divs[13]=divs[13]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INP18M))])
      mean_NB_19ANSMOINS[i]=mean_NB_19ANSMOINS[i]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INP19M))]*Recensement$INP19M[which(indices & !is.na(Recensement$INP19M))])
      divs[14]=divs[14]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INP19M))])
      mean_NB_24ANSMOINS[i]=mean_NB_24ANSMOINS[i]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INP24M))]*Recensement$INP24M[which(indices & !is.na(Recensement$INP24M))])
      divs[15]=divs[15]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INP24M))])
      mean_NB_3ANSMOINS[i]=mean_NB_3ANSMOINS[i]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INP3M))]*Recensement$INP3M[which(indices & !is.na(Recensement$INP3M))])
      divs[16]=divs[16]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INP3M))])
      mean_NB_60ANSPLUS[i]=mean_NB_60ANSPLUS[i]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INP60M))]*Recensement$INP60M[which(indices & !is.na(Recensement$INP60M))])
      divs[17]=divs[17]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INP60M))])
      mean_NB_65ANSPLUS[i]=mean_NB_65ANSPLUS[i]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INP65M))]*Recensement$INP65M[which(indices & !is.na(Recensement$INP65M))])
      divs[18]=divs[18]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INP65M))])
      mean_NB_6ANSMOINS[i]=mean_NB_6ANSMOINS[i]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INP6M))]*Recensement$INP6M[which(indices & !is.na(Recensement$INP6M))])
      divs[19]=divs[19]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INP6M))])
      mean_NB_75ANSPLUS[i]=mean_NB_75ANSPLUS[i]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INP75M))]*Recensement$INP75M[which(indices & !is.na(Recensement$INP75M))])
      divs[20]=divs[20]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INP75M))])
      mean_NB_PERSONNES_ACTIVES[i]=mean_NB_PERSONNES_ACTIVES[i]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INPAM))]*Recensement$INPAM[which(indices & !is.na(Recensement$INPAM))])
      divs[21]=divs[21]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INPAM))])
      mean_NB_PERSONNES[i]=mean_NB_PERSONNES[i]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INPER))]*Recensement$INPER[which(indices & !is.na(Recensement$INPER))])
      divs[22]=divs[22]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INPER))])
      mean_NB_PERSONNES_MASCULIN[i]=mean_NB_PERSONNES_MASCULIN[i]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INPER1))]*Recensement$INPER1[which(indices & !is.na(Recensement$INPER1))])
      divs[23]=divs[23]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INPER1))])
      mean_NB_PERSONNES_FEMININ[i]=mean_NB_PERSONNES_FEMININ[i]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INPER2))]*Recensement$INPER2[which(indices & !is.na(Recensement$INPER2))])
      divs[24]=divs[24]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INPER2))])
      mean_NB_PERSONNES_ACTIVES_AVEC_EMPLOI[i]=mean_NB_PERSONNES_ACTIVES_AVEC_EMPLOI[i]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INPOM))]*Recensement$INPOM[which(indices & !is.na(Recensement$INPOM))])
      divs[25]=divs[25]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INPOM))])
      mean_NB_PERSONNES_SCOLARISEES[i]=mean_NB_PERSONNES_SCOLARISEES[i]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INPSM))]*Recensement$INPSM[which(indices & !is.na(Recensement$INPSM))])
      divs[26]=divs[26]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INPSM))])
      mean_NB_PIECES_LOGEMENT[i]=mean_NB_PIECES_LOGEMENT[i]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$NBPI))]*Recensement$NBPI[which(indices & !is.na(Recensement$NBPI))])
      divs[27]=divs[27]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$NBPI))])
      pourc_PROPRIETAIRE_LOGEMENT[i]=pourc_PROPRIETAIRE_LOGEMENT[i]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$STOCD=="10"))])
      divs[28]=divs[28]+weightIris*sum(Recensement$IPONDL[which(indices & !(Recensement$STOCD=="ZZ"))])
      pourc_LOCATAIRE_LGMNT_VIDE_NON_HLM[i]=pourc_LOCATAIRE_LGMNT_VIDE_NON_HLM[i]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$STOCD=="21"))])
      pourc_LOCATAIRE_LGMNT_VIDE_HLM[i]=pourc_LOCATAIRE_LGMNT_VIDE_HLM[i]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$STOCD=="22"))])
      pourc_LOCATAIRE_MEUBLE[i]=pourc_LOCATAIRE_MEUBLE[i]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$STOCD=="23"))])
      pourc_LOGE_GRATUITEMENT[i]=pourc_LOGE_GRATUITEMENT[i]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$STOCD=="30"))])
      divs[29]=divs[29]+weightIris*sum(Recensement$IPONDL[which(indices & !(Recensement$STOCD=="ZZ"))])
      pourc_LOGMNT_MOINS_40M2[i]=pourc_LOGMNT_MOINS_40M2[i]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$SURF==1))])
      pourc_LOGMNT_40M2_A_100M2[i]=pourc_LOGMNT_40M2_A_100M2[i]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$SURF==2))])
      pourc_LOGMNT_PLUS_100M2[i]=pourc_LOGMNT_PLUS_100M2[i]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$SURF==3))])
      divs[30]=divs[30]+weightIris*sum(Recensement$IPONDL[which(indices & !(Recensement$SURF=="Z"))])
      pourc_REFERENT_ACTIFS[i]=pourc_REFERENT_ACTIFS[i]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$TACTM=="11"))])
      pourc_REFERENT_CHOMEUR[i]=pourc_REFERENT_CHOMEUR[i]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$TACTM=="12"))])
      pourc_REFERENT_RETRAITE[i]=pourc_REFERENT_RETRAITE[i]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$TACTM=="21"))])
      pourc_REFERENT_ETUDIANT[i]=pourc_REFERENT_ETUDIANT[i]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$TACTM=="22"))])
      pourc_REFERENT_MOINS_14ANS[i]=pourc_REFERENT_MOINS_14ANS[i]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$TACTM=="23"))])
      pourc_REFERENT_PERSONNE_AU_FOYER[i]=pourc_REFERENT_PERSONNE_AU_FOYER[i]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$TACTM=="24"))])
      pourc_REFERENT_AUTRES_INACTIFS[i]=pourc_REFERENT_AUTRES_INACTIFS[i]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$TACTM=="25"))])
      divs[31]=divs[31]+weightIris*sum(Recensement$IPONDL[which(indices & !(Recensement$TACTM=="ZZ") & !(Recensement$TACTM=="YY"))])
      pourc_LOGEMENT_ISOLE[i]=pourc_LOGEMENT_ISOLE[i]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$TYPC==1))])
      pourc_LOGEMENT_JUMELE[i]=pourc_LOGEMENT_JUMELE[i]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$TYPC==2))])
      pourc_BATIMENT_2LOGEMENTS_ET_PLUS[i]=pourc_BATIMENT_2LOGEMENTS_ET_PLUS[i]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$TYPC==3))])
      pourc_BATIMENT_NON_HABITATION[i]=pourc_BATIMENT_NON_HABITATION[i]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$TYPC==4))])
      pourc_CONSTRUCTION_PROVISOIRE[i]=pourc_CONSTRUCTION_PROVISOIRE[i]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$TYPC==5))])
      divs[32]=divs[32]+weightIris*sum(Recensement$IPONDL[which(indices & !(Recensement$TYPC=="Z"))])
      pourc_MAISONS[i]=pourc_MAISONS[i]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$TYPL==1))])
      pourc_APPARTEMENTS[i]=pourc_APPARTEMENTS[i]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$TYPL==2))])
      pourc_LOGEMENT_FOYER[i]=pourc_LOGEMENT_FOYER[i]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$TYPL==3))])
      pourc_CHAMBRE_HOTEL[i]=pourc_CHAMBRE_HOTEL[i]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$TYPL==4))])
      pourc_HABITATION_DE_FORTUNE[i]=pourc_HABITATION_DE_FORTUNE[i]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$TYPL==5))])
      pourc_PIECE_INDEPENDANTE[i]=pourc_PIECE_INDEPENDANTE[i]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$TYPL==6))])
      divs[33]=divs[33]+weightIris*sum(Recensement$IPONDL[which(indices & !(Recensement$TYPL=="Z"))])
    }
  }
  pourc_HLM[i]=pourc_HLM[i]/divs[1]
  pourc_IMMIGRATION[i]=pourc_IMMIGRATION[i]/divs[2]
  mean_ANNEE_EMMENAGEMENT[i]=mean_ANNEE_EMMENAGEMENT[i]/divs[3]
  pourc_ASCENSEUR[i]=pourc_ASCENSEUR[i]/divs[4]
  pourc_RESIDENCE_PRINCIPALE[i]=pourc_RESIDENCE_PRINCIPALE[i]/divs[5]
  pourc_LOGEMENT_OCCASIONNEL[i]=pourc_LOGEMENT_OCCASIONNEL[i]/divs[5]
  pourc_RESIDENCE_SECONDAIRE[i]=pourc_RESIDENCE_SECONDAIRE[i]/divs[5]
  mean_AGE[i]=mean_AGE[i]/divs[6]
  pourc_CHAUFFAGE_CENTRAL_COLLECTIF[i]=pourc_CHAUFFAGE_CENTRAL_COLLECTIF[i]/divs[7]
  pourc_CHAUFFAGE_CENTRAL_INDIV[i]=pourc_CHAUFFAGE_CENTRAL_INDIV[i]/divs[7]
  pourc_CHAUFFAGE_ELECTRIQUE[i]=pourc_CHAUFFAGE_ELECTRIQUE[i]/divs[7]
  pourc_COMBUSTIBLE_ELECTRICITE[i]=pourc_COMBUSTIBLE_ELECTRICITE[i]/divs[8]
  pourc_STATIONNEMENT[i]=pourc_STATIONNEMENT[i]/divs[9]
  mean_NB_ELEVES_ETUDIANTS_14ANSPLUS[i]=mean_NB_ELEVES_ETUDIANTS_14ANSPLUS[i]/divs[10]
  mean_NB_11ANSMOINS[i]=mean_NB_11ANSMOINS[i]/divs[11]
  mean_NB_16ANSMOINS[i]=mean_NB_16ANSMOINS[i]/divs[12]
  mean_NB_18ANSMOINS[i]=mean_NB_18ANSMOINS[i]/divs[13]
  mean_NB_19ANSMOINS[i]=mean_NB_19ANSMOINS[i]/divs[14]
  mean_NB_24ANSMOINS[i]=mean_NB_24ANSMOINS[i]/divs[15]
  mean_NB_3ANSMOINS[i]=mean_NB_3ANSMOINS[i]/divs[16]
  mean_NB_60ANSPLUS[i]=mean_NB_60ANSPLUS[i]/divs[17]
  mean_NB_65ANSPLUS[i]=mean_NB_65ANSPLUS[i]/divs[18]
  mean_NB_6ANSMOINS[i]=mean_NB_6ANSMOINS[i]/divs[19]
  mean_NB_75ANSPLUS[i]=mean_NB_75ANSPLUS[i]/divs[20]
  mean_NB_PERSONNES_ACTIVES[i]=mean_NB_PERSONNES_ACTIVES[i]/divs[21]
  mean_NB_PERSONNES[i]=mean_NB_PERSONNES[i]/divs[22]
  mean_NB_PERSONNES_MASCULIN[i]=mean_NB_PERSONNES_MASCULIN[i]/divs[23]
  mean_NB_PERSONNES_FEMININ[i]=mean_NB_PERSONNES_FEMININ[i]/divs[24]
  mean_NB_PERSONNES_ACTIVES_AVEC_EMPLOI[i]=mean_NB_PERSONNES_ACTIVES_AVEC_EMPLOI[i]/divs[25]
  mean_NB_PERSONNES_SCOLARISEES[i]=mean_NB_PERSONNES_SCOLARISEES[i]/divs[26]
  mean_NB_PIECES_LOGEMENT[i]=mean_NB_PIECES_LOGEMENT[i]/divs[27]
  pourc_PROPRIETAIRE_LOGEMENT[i]=pourc_PROPRIETAIRE_LOGEMENT[i]/divs[28]
  pourc_LOCATAIRE_LGMNT_VIDE_NON_HLM[i]=pourc_LOCATAIRE_LGMNT_VIDE_NON_HLM[i]/divs[29]
  pourc_LOCATAIRE_LGMNT_VIDE_HLM[i]=pourc_LOCATAIRE_LGMNT_VIDE_HLM[i]/divs[29]
  pourc_LOCATAIRE_MEUBLE[i]=pourc_LOCATAIRE_MEUBLE[i]/divs[29]
  pourc_LOGE_GRATUITEMENT[i]=pourc_LOGE_GRATUITEMENT[i]/divs[29]
  pourc_LOGMNT_MOINS_40M2[i]=pourc_LOGMNT_MOINS_40M2[i]/divs[30]
  pourc_LOGMNT_40M2_A_100M2[i]=pourc_LOGMNT_40M2_A_100M2[i]/divs[30]
  pourc_LOGMNT_PLUS_100M2[i]=pourc_LOGMNT_PLUS_100M2[i]/divs[30]
  pourc_REFERENT_ACTIFS[i]=pourc_REFERENT_ACTIFS[i]/divs[31]
  pourc_REFERENT_CHOMEUR[i]=pourc_REFERENT_CHOMEUR[i]/divs[31]
  pourc_REFERENT_RETRAITE[i]=pourc_REFERENT_RETRAITE[i]/divs[31]
  pourc_REFERENT_ETUDIANT[i]=pourc_REFERENT_ETUDIANT[i]/divs[31]
  pourc_REFERENT_MOINS_14ANS[i]=pourc_REFERENT_MOINS_14ANS[i]/divs[31]
  pourc_REFERENT_PERSONNE_AU_FOYER[i]=pourc_REFERENT_PERSONNE_AU_FOYER[i]/divs[31]
  pourc_REFERENT_AUTRES_INACTIFS[i]=pourc_REFERENT_AUTRES_INACTIFS[i]/divs[31]
  pourc_LOGEMENT_ISOLE[i]=pourc_LOGEMENT_ISOLE[i]/divs[32]
  pourc_LOGEMENT_JUMELE[i]=pourc_LOGEMENT_JUMELE[i]/divs[32]
  pourc_BATIMENT_2LOGEMENTS_ET_PLUS[i]=pourc_BATIMENT_2LOGEMENTS_ET_PLUS[i]/divs[32]
  pourc_BATIMENT_NON_HABITATION[i]=pourc_BATIMENT_NON_HABITATION[i]/divs[32]
  pourc_CONSTRUCTION_PROVISOIRE[i]=pourc_CONSTRUCTION_PROVISOIRE[i]/divs[32]
  pourc_MAISONS[i]=pourc_MAISONS[i]/divs[33]
  pourc_APPARTEMENTS[i]=pourc_APPARTEMENTS[i]/divs[33]
  pourc_LOGEMENT_FOYER[i]=pourc_LOGEMENT_FOYER[i]/divs[33]
  pourc_CHAMBRE_HOTEL[i]=pourc_CHAMBRE_HOTEL[i]/divs[33]
  pourc_HABITATION_DE_FORTUNE[i]=pourc_HABITATION_DE_FORTUNE[i]/divs[33]
  pourc_PIECE_INDEPENDANTE[i]=pourc_PIECE_INDEPENDANTE[i]/divs[33]
  setTxtProgressBar(pb, i)
}

pb <- txtProgressBar(min=0, max=nb_PS, style = 3)
setTxtProgressBar(pb, 0)

for(i in 1:nb_PS) {
  divs=array(0,33)
  ps_cur=PS_names[i]
  irisS=unique(Customers$IRIS[which(substr(Customers$CODE_GDO,1,5)==ps_cur)])
  irisS=irisS[which(!is.na(irisS))]
  poste_source[i+nb_HTA]=ps_cur
  irisS = irisS[!is.na(irisS)]
  irisS = irisS[which(irisS %in% Recensement$IRIS_completed)]
  nb_IRIS[i+nb_HTA]=length(irisS)
  weightIris=0
  if(length(irisS)!=0) {
    for(j in 1:length(irisS)) {
      weightIris=length(which(substr(Customers$CODE_GDO,1,5)==ps_cur & Customers$IRIS==irisS[j]))/length(which(Customers$IRIS==irisS[j]))
      indices=(Recensement$IRIS_completed==irisS[j])
      pourc_HLM[i+nb_HTA]=pourc_HLM[i+nb_HTA]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$HLML==1))])
      divs[1]=divs[1]+weightIris*sum(Recensement$IPONDL[which(indices & (Recensement$HLML==1 | Recensement$HLML==2))])
      pourc_IMMIGRATION[i+nb_HTA]=pourc_IMMIGRATION[i+nb_HTA]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$IMMIM==1))])
      divs[2]=divs[2]+weightIris*sum(Recensement$IPONDL[which(indices & (Recensement$IMMIM==1 | Recensement$IMMIM==2))])
      mean_ANNEE_EMMENAGEMENT[i+nb_HTA]=mean_ANNEE_EMMENAGEMENT[i+nb_HTA]+weightIris*sum(Recensement$IPONDL[which(indices & (Recensement$AEMM!=0))]*Recensement$AEMM[which(indices & (Recensement$AEMM!=0))])
      divs[3]=divs[3]+weightIris*sum(Recensement$IPONDL[which(indices & (Recensement$AEMM!=0))])
      pourc_ASCENSEUR[i+nb_HTA]=pourc_ASCENSEUR[i+nb_HTA]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$ASCEN==1))])
      divs[4]=divs[4]+weightIris*sum(Recensement$IPONDL[which(indices & (Recensement$ASCEN==1 | Recensement$ASCEN==2))])
      pourc_RESIDENCE_PRINCIPALE[i+nb_HTA]=pourc_RESIDENCE_PRINCIPALE[i+nb_HTA]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$CATL==1))])
      pourc_LOGEMENT_OCCASIONNEL[i+nb_HTA]=pourc_LOGEMENT_OCCASIONNEL[i+nb_HTA]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$CATL==2))])
      pourc_RESIDENCE_SECONDAIRE[i+nb_HTA]=pourc_RESIDENCE_SECONDAIRE[i+nb_HTA]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$CATL==3))])
      divs[5]=divs[5]+weightIris*sum(Recensement$IPONDL[which(indices & (Recensement$CATL==1 | Recensement$CATL==2 | Recensement$CATL==3 | Recensement$CATL==4))])
      mean_AGE[i+nb_HTA]=mean_AGE[i+nb_HTA]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$AGEMEN8))]*Recensement$AGEMEN8[which(indices & !is.na(Recensement$AGEMEN8))])
      divs[6]=divs[6]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$AGEMEN8))])
      pourc_CHAUFFAGE_CENTRAL_COLLECTIF[i+nb_HTA]=pourc_CHAUFFAGE_CENTRAL_COLLECTIF[i+nb_HTA]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$CHFL==1))])
      pourc_CHAUFFAGE_CENTRAL_INDIV[i+nb_HTA]=pourc_CHAUFFAGE_CENTRAL_INDIV[i+nb_HTA]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$CHFL==2))])
      pourc_CHAUFFAGE_ELECTRIQUE[i+nb_HTA]=pourc_CHAUFFAGE_ELECTRIQUE[i+nb_HTA]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$CHFL==3))])
      divs[7]=divs[7]+weightIris*sum(Recensement$IPONDL[which(indices & (Recensement$CHFL==1 | Recensement$CHFL==2 | Recensement$CHFL==3 | Recensement$CHFL==4))])
      pourc_COMBUSTIBLE_ELECTRICITE[i+nb_HTA]=pourc_COMBUSTIBLE_ELECTRICITE[i+nb_HTA]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$CMBL==4))])
      divs[8]=divs[8]+weightIris*sum(Recensement$IPONDL[which(indices & (Recensement$CMBL==1 | Recensement$CMBL==2 | Recensement$CMBL==3 | Recensement$CMBL==4 | Recensement$CMBL==5 | Recensement$CMBL==6))])
      pourc_STATIONNEMENT[i+nb_HTA]=pourc_STATIONNEMENT[i+nb_HTA]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$GARL==1))])
      divs[9]=divs[9]+weightIris*sum(Recensement$IPONDL[which(indices & (Recensement$GARL==1 | Recensement$GARL==2))])
      mean_NB_ELEVES_ETUDIANTS_14ANSPLUS[i+nb_HTA]=mean_NB_ELEVES_ETUDIANTS_14ANSPLUS[i+nb_HTA]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INEEM))]*Recensement$INEEM[which(indices & !is.na(Recensement$INEEM))])
      divs[10]=divs[10]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INEEM))])
      mean_NB_11ANSMOINS[i+nb_HTA]=mean_NB_11ANSMOINS[i+nb_HTA]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INP11M))]*Recensement$INP11M[which(indices & !is.na(Recensement$INP11M))])
      divs[11]=divs[11]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INP11M))])
      mean_NB_16ANSMOINS[i+nb_HTA]=mean_NB_16ANSMOINS[i+nb_HTA]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INP16M))]*Recensement$INP16M[which(indices & !is.na(Recensement$INP16M))])
      divs[12]=divs[12]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INP16M))])
      mean_NB_18ANSMOINS[i+nb_HTA]=mean_NB_18ANSMOINS[i+nb_HTA]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INP18M))]*Recensement$INP18M[which(indices & !is.na(Recensement$INP18M))])
      divs[13]=divs[13]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INP18M))])
      mean_NB_19ANSMOINS[i+nb_HTA]=mean_NB_19ANSMOINS[i+nb_HTA]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INP19M))]*Recensement$INP19M[which(indices & !is.na(Recensement$INP19M))])
      divs[14]=divs[14]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INP19M))])
      mean_NB_24ANSMOINS[i+nb_HTA]=mean_NB_24ANSMOINS[i+nb_HTA]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INP24M))]*Recensement$INP24M[which(indices & !is.na(Recensement$INP24M))])
      divs[15]=divs[15]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INP24M))])
      mean_NB_3ANSMOINS[i+nb_HTA]=mean_NB_3ANSMOINS[i+nb_HTA]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INP3M))]*Recensement$INP3M[which(indices & !is.na(Recensement$INP3M))])
      divs[16]=divs[16]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INP3M))])
      mean_NB_60ANSPLUS[i+nb_HTA]=mean_NB_60ANSPLUS[i+nb_HTA]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INP60M))]*Recensement$INP60M[which(indices & !is.na(Recensement$INP60M))])
      divs[17]=divs[17]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INP60M))])
      mean_NB_65ANSPLUS[i+nb_HTA]=mean_NB_65ANSPLUS[i+nb_HTA]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INP65M))]*Recensement$INP65M[which(indices & !is.na(Recensement$INP65M))])
      divs[18]=divs[18]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INP65M))])
      mean_NB_6ANSMOINS[i+nb_HTA]=mean_NB_6ANSMOINS[i+nb_HTA]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INP6M))]*Recensement$INP6M[which(indices & !is.na(Recensement$INP6M))])
      divs[19]=divs[19]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INP6M))])
      mean_NB_75ANSPLUS[i+nb_HTA]=mean_NB_75ANSPLUS[i+nb_HTA]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INP75M))]*Recensement$INP75M[which(indices & !is.na(Recensement$INP75M))])
      divs[20]=divs[20]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INP75M))])
      mean_NB_PERSONNES_ACTIVES[i+nb_HTA]=mean_NB_PERSONNES_ACTIVES[i+nb_HTA]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INPAM))]*Recensement$INPAM[which(indices & !is.na(Recensement$INPAM))])
      divs[21]=divs[21]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INPAM))])
      mean_NB_PERSONNES[i+nb_HTA]=mean_NB_PERSONNES[i+nb_HTA]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INPER))]*Recensement$INPER[which(indices & !is.na(Recensement$INPER))])
      divs[22]=divs[22]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INPER))])
      mean_NB_PERSONNES_MASCULIN[i+nb_HTA]=mean_NB_PERSONNES_MASCULIN[i+nb_HTA]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INPER1))]*Recensement$INPER1[which(indices & !is.na(Recensement$INPER1))])
      divs[23]=divs[23]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INPER1))])
      mean_NB_PERSONNES_FEMININ[i+nb_HTA]=mean_NB_PERSONNES_FEMININ[i+nb_HTA]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INPER2))]*Recensement$INPER2[which(indices & !is.na(Recensement$INPER2))])
      divs[24]=divs[24]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INPER2))])
      mean_NB_PERSONNES_ACTIVES_AVEC_EMPLOI[i+nb_HTA]=mean_NB_PERSONNES_ACTIVES_AVEC_EMPLOI[i+nb_HTA]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INPOM))]*Recensement$INPOM[which(indices & !is.na(Recensement$INPOM))])
      divs[25]=divs[25]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INPOM))])
      mean_NB_PERSONNES_SCOLARISEES[i+nb_HTA]=mean_NB_PERSONNES_SCOLARISEES[i+nb_HTA]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INPSM))]*Recensement$INPSM[which(indices & !is.na(Recensement$INPSM))])
      divs[26]=divs[26]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$INPSM))])
      mean_NB_PIECES_LOGEMENT[i+nb_HTA]=mean_NB_PIECES_LOGEMENT[i+nb_HTA]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$NBPI))]*Recensement$NBPI[which(indices & !is.na(Recensement$NBPI))])
      divs[27]=divs[27]+weightIris*sum(Recensement$IPONDL[which(indices & !is.na(Recensement$NBPI))])
      pourc_PROPRIETAIRE_LOGEMENT[i+nb_HTA]=pourc_PROPRIETAIRE_LOGEMENT[i+nb_HTA]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$STOCD=="10"))])
      divs[28]=divs[28]+weightIris*sum(Recensement$IPONDL[which(indices & !(Recensement$STOCD=="ZZ"))])
      pourc_LOCATAIRE_LGMNT_VIDE_NON_HLM[i+nb_HTA]=pourc_LOCATAIRE_LGMNT_VIDE_NON_HLM[i+nb_HTA]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$STOCD=="21"))])
      pourc_LOCATAIRE_LGMNT_VIDE_HLM[i+nb_HTA]=pourc_LOCATAIRE_LGMNT_VIDE_HLM[i+nb_HTA]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$STOCD=="22"))])
      pourc_LOCATAIRE_MEUBLE[i+nb_HTA]=pourc_LOCATAIRE_MEUBLE[i+nb_HTA]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$STOCD=="23"))])
      pourc_LOGE_GRATUITEMENT[i+nb_HTA]=pourc_LOGE_GRATUITEMENT[i+nb_HTA]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$STOCD=="30"))])
      divs[29]=divs[29]+weightIris*sum(Recensement$IPONDL[which(indices & !(Recensement$STOCD=="ZZ"))])
      pourc_LOGMNT_MOINS_40M2[i+nb_HTA]=pourc_LOGMNT_MOINS_40M2[i+nb_HTA]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$SURF==1))])
      pourc_LOGMNT_40M2_A_100M2[i+nb_HTA]=pourc_LOGMNT_40M2_A_100M2[i+nb_HTA]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$SURF==2))])
      pourc_LOGMNT_PLUS_100M2[i+nb_HTA]=pourc_LOGMNT_PLUS_100M2[i+nb_HTA]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$SURF==3))])
      divs[30]=divs[30]+weightIris*sum(Recensement$IPONDL[which(indices & !(Recensement$SURF=="Z"))])
      pourc_REFERENT_ACTIFS[i+nb_HTA]=pourc_REFERENT_ACTIFS[i+nb_HTA]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$TACTM=="11"))])
      pourc_REFERENT_CHOMEUR[i+nb_HTA]=pourc_REFERENT_CHOMEUR[i+nb_HTA]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$TACTM=="12"))])
      pourc_REFERENT_RETRAITE[i+nb_HTA]=pourc_REFERENT_RETRAITE[i+nb_HTA]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$TACTM=="21"))])
      pourc_REFERENT_ETUDIANT[i+nb_HTA]=pourc_REFERENT_ETUDIANT[i+nb_HTA]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$TACTM=="22"))])
      pourc_REFERENT_MOINS_14ANS[i+nb_HTA]=pourc_REFERENT_MOINS_14ANS[i+nb_HTA]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$TACTM=="23"))])
      pourc_REFERENT_PERSONNE_AU_FOYER[i+nb_HTA]=pourc_REFERENT_PERSONNE_AU_FOYER[i+nb_HTA]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$TACTM=="24"))])
      pourc_REFERENT_AUTRES_INACTIFS[i+nb_HTA]=pourc_REFERENT_AUTRES_INACTIFS[i+nb_HTA]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$TACTM=="25"))])
      divs[31]=divs[31]+weightIris*sum(Recensement$IPONDL[which(indices & !(Recensement$TACTM=="ZZ") & !(Recensement$TACTM=="YY"))])
      pourc_LOGEMENT_ISOLE[i+nb_HTA]=pourc_LOGEMENT_ISOLE[i+nb_HTA]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$TYPC==1))])
      pourc_LOGEMENT_JUMELE[i+nb_HTA]=pourc_LOGEMENT_JUMELE[i+nb_HTA]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$TYPC==2))])
      pourc_BATIMENT_2LOGEMENTS_ET_PLUS[i+nb_HTA]=pourc_BATIMENT_2LOGEMENTS_ET_PLUS[i+nb_HTA]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$TYPC==3))])
      pourc_BATIMENT_NON_HABITATION[i+nb_HTA]=pourc_BATIMENT_NON_HABITATION[i+nb_HTA]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$TYPC==4))])
      pourc_CONSTRUCTION_PROVISOIRE[i+nb_HTA]=pourc_CONSTRUCTION_PROVISOIRE[i+nb_HTA]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$TYPC==5))])
      divs[32]=divs[32]+weightIris*sum(Recensement$IPONDL[which(indices & !(Recensement$TYPC=="Z"))])
      pourc_MAISONS[i+nb_HTA]=pourc_MAISONS[i+nb_HTA]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$TYPL==1))])
      pourc_APPARTEMENTS[i+nb_HTA]=pourc_APPARTEMENTS[i+nb_HTA]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$TYPL==2))])
      pourc_LOGEMENT_FOYER[i+nb_HTA]=pourc_LOGEMENT_FOYER[i+nb_HTA]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$TYPL==3))])
      pourc_CHAMBRE_HOTEL[i+nb_HTA]=pourc_CHAMBRE_HOTEL[i+nb_HTA]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$TYPL==4))])
      pourc_HABITATION_DE_FORTUNE[i+nb_HTA]=pourc_HABITATION_DE_FORTUNE[i+nb_HTA]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$TYPL==5))])
      pourc_PIECE_INDEPENDANTE[i+nb_HTA]=pourc_PIECE_INDEPENDANTE[i+nb_HTA]+weightIris*100*sum(Recensement$IPONDL[which(indices & (Recensement$TYPL==6))])
      divs[33]=divs[33]+weightIris*sum(Recensement$IPONDL[which(indices & !(Recensement$TYPL=="Z"))])
    }
  }
  pourc_HLM[i+nb_HTA]=pourc_HLM[i+nb_HTA]/divs[1]
  pourc_IMMIGRATION[i+nb_HTA]=pourc_IMMIGRATION[i+nb_HTA]/divs[2]
  mean_ANNEE_EMMENAGEMENT[i+nb_HTA]=mean_ANNEE_EMMENAGEMENT[i+nb_HTA]/divs[3]
  pourc_ASCENSEUR[i+nb_HTA]=pourc_ASCENSEUR[i+nb_HTA]/divs[4]
  pourc_RESIDENCE_PRINCIPALE[i+nb_HTA]=pourc_RESIDENCE_PRINCIPALE[i+nb_HTA]/divs[5]
  pourc_LOGEMENT_OCCASIONNEL[i+nb_HTA]=pourc_LOGEMENT_OCCASIONNEL[i+nb_HTA]/divs[5]
  pourc_RESIDENCE_SECONDAIRE[i+nb_HTA]=pourc_RESIDENCE_SECONDAIRE[i+nb_HTA]/divs[5]
  mean_AGE[i+nb_HTA]=mean_AGE[i+nb_HTA]/divs[6]
  pourc_CHAUFFAGE_CENTRAL_COLLECTIF[i+nb_HTA]=pourc_CHAUFFAGE_CENTRAL_COLLECTIF[i+nb_HTA]/divs[7]
  pourc_CHAUFFAGE_CENTRAL_INDIV[i+nb_HTA]=pourc_CHAUFFAGE_CENTRAL_INDIV[i+nb_HTA]/divs[7]
  pourc_CHAUFFAGE_ELECTRIQUE[i+nb_HTA]=pourc_CHAUFFAGE_ELECTRIQUE[i+nb_HTA]/divs[7]
  pourc_COMBUSTIBLE_ELECTRICITE[i+nb_HTA]=pourc_COMBUSTIBLE_ELECTRICITE[i+nb_HTA]/divs[8]
  pourc_STATIONNEMENT[i+nb_HTA]=pourc_STATIONNEMENT[i+nb_HTA]/divs[9]
  mean_NB_ELEVES_ETUDIANTS_14ANSPLUS[i+nb_HTA]=mean_NB_ELEVES_ETUDIANTS_14ANSPLUS[i+nb_HTA]/divs[10]
  mean_NB_11ANSMOINS[i+nb_HTA]=mean_NB_11ANSMOINS[i+nb_HTA]/divs[11]
  mean_NB_16ANSMOINS[i+nb_HTA]=mean_NB_16ANSMOINS[i+nb_HTA]/divs[12]
  mean_NB_18ANSMOINS[i+nb_HTA]=mean_NB_18ANSMOINS[i+nb_HTA]/divs[13]
  mean_NB_19ANSMOINS[i+nb_HTA]=mean_NB_19ANSMOINS[i+nb_HTA]/divs[14]
  mean_NB_24ANSMOINS[i+nb_HTA]=mean_NB_24ANSMOINS[i+nb_HTA]/divs[15]
  mean_NB_3ANSMOINS[i+nb_HTA]=mean_NB_3ANSMOINS[i+nb_HTA]/divs[16]
  mean_NB_60ANSPLUS[i+nb_HTA]=mean_NB_60ANSPLUS[i+nb_HTA]/divs[17]
  mean_NB_65ANSPLUS[i+nb_HTA]=mean_NB_65ANSPLUS[i+nb_HTA]/divs[18]
  mean_NB_6ANSMOINS[i+nb_HTA]=mean_NB_6ANSMOINS[i+nb_HTA]/divs[19]
  mean_NB_75ANSPLUS[i+nb_HTA]=mean_NB_75ANSPLUS[i+nb_HTA]/divs[20]
  mean_NB_PERSONNES_ACTIVES[i+nb_HTA]=mean_NB_PERSONNES_ACTIVES[i+nb_HTA]/divs[21]
  mean_NB_PERSONNES[i+nb_HTA]=mean_NB_PERSONNES[i+nb_HTA]/divs[22]
  mean_NB_PERSONNES_MASCULIN[i+nb_HTA]=mean_NB_PERSONNES_MASCULIN[i+nb_HTA]/divs[23]
  mean_NB_PERSONNES_FEMININ[i+nb_HTA]=mean_NB_PERSONNES_FEMININ[i+nb_HTA]/divs[24]
  mean_NB_PERSONNES_ACTIVES_AVEC_EMPLOI[i+nb_HTA]=mean_NB_PERSONNES_ACTIVES_AVEC_EMPLOI[i+nb_HTA]/divs[25]
  mean_NB_PERSONNES_SCOLARISEES[i+nb_HTA]=mean_NB_PERSONNES_SCOLARISEES[i+nb_HTA]/divs[26]
  mean_NB_PIECES_LOGEMENT[i+nb_HTA]=mean_NB_PIECES_LOGEMENT[i+nb_HTA]/divs[27]
  pourc_PROPRIETAIRE_LOGEMENT[i+nb_HTA]=pourc_PROPRIETAIRE_LOGEMENT[i+nb_HTA]/divs[28]
  pourc_LOCATAIRE_LGMNT_VIDE_NON_HLM[i+nb_HTA]=pourc_LOCATAIRE_LGMNT_VIDE_NON_HLM[i+nb_HTA]/divs[29]
  pourc_LOCATAIRE_LGMNT_VIDE_HLM[i+nb_HTA]=pourc_LOCATAIRE_LGMNT_VIDE_HLM[i+nb_HTA]/divs[29]
  pourc_LOCATAIRE_MEUBLE[i+nb_HTA]=pourc_LOCATAIRE_MEUBLE[i+nb_HTA]/divs[29]
  pourc_LOGE_GRATUITEMENT[i+nb_HTA]=pourc_LOGE_GRATUITEMENT[i+nb_HTA]/divs[29]
  pourc_LOGMNT_MOINS_40M2[i+nb_HTA]=pourc_LOGMNT_MOINS_40M2[i+nb_HTA]/divs[30]
  pourc_LOGMNT_40M2_A_100M2[i+nb_HTA]=pourc_LOGMNT_40M2_A_100M2[i+nb_HTA]/divs[30]
  pourc_LOGMNT_PLUS_100M2[i+nb_HTA]=pourc_LOGMNT_PLUS_100M2[i+nb_HTA]/divs[30]
  pourc_REFERENT_ACTIFS[i+nb_HTA]=pourc_REFERENT_ACTIFS[i+nb_HTA]/divs[31]
  pourc_REFERENT_CHOMEUR[i+nb_HTA]=pourc_REFERENT_CHOMEUR[i+nb_HTA]/divs[31]
  pourc_REFERENT_RETRAITE[i+nb_HTA]=pourc_REFERENT_RETRAITE[i+nb_HTA]/divs[31]
  pourc_REFERENT_ETUDIANT[i+nb_HTA]=pourc_REFERENT_ETUDIANT[i+nb_HTA]/divs[31]
  pourc_REFERENT_MOINS_14ANS[i+nb_HTA]=pourc_REFERENT_MOINS_14ANS[i+nb_HTA]/divs[31]
  pourc_REFERENT_PERSONNE_AU_FOYER[i+nb_HTA]=pourc_REFERENT_PERSONNE_AU_FOYER[i+nb_HTA]/divs[31]
  pourc_REFERENT_AUTRES_INACTIFS[i+nb_HTA]=pourc_REFERENT_AUTRES_INACTIFS[i+nb_HTA]/divs[31]
  pourc_LOGEMENT_ISOLE[i+nb_HTA]=pourc_LOGEMENT_ISOLE[i+nb_HTA]/divs[32]
  pourc_LOGEMENT_JUMELE[i+nb_HTA]=pourc_LOGEMENT_JUMELE[i+nb_HTA]/divs[32]
  pourc_BATIMENT_2LOGEMENTS_ET_PLUS[i+nb_HTA]=pourc_BATIMENT_2LOGEMENTS_ET_PLUS[i+nb_HTA]/divs[32]
  pourc_BATIMENT_NON_HABITATION[i+nb_HTA]=pourc_BATIMENT_NON_HABITATION[i+nb_HTA]/divs[32]
  pourc_CONSTRUCTION_PROVISOIRE[i+nb_HTA]=pourc_CONSTRUCTION_PROVISOIRE[i+nb_HTA]/divs[32]
  pourc_MAISONS[i+nb_HTA]=pourc_MAISONS[i+nb_HTA]/divs[33]
  pourc_APPARTEMENTS[i+nb_HTA]=pourc_APPARTEMENTS[i+nb_HTA]/divs[33]
  pourc_LOGEMENT_FOYER[i+nb_HTA]=pourc_LOGEMENT_FOYER[i+nb_HTA]/divs[33]
  pourc_CHAMBRE_HOTEL[i+nb_HTA]=pourc_CHAMBRE_HOTEL[i+nb_HTA]/divs[33]
  pourc_HABITATION_DE_FORTUNE[i+nb_HTA]=pourc_HABITATION_DE_FORTUNE[i+nb_HTA]/divs[33]
  pourc_PIECE_INDEPENDANTE[i+nb_HTA]=pourc_PIECE_INDEPENDANTE[i+nb_HTA]/divs[33]
  setTxtProgressBar(pb, i)
}

IndicateursINSEE = data.frame(
  HTA = All_names,
  poste_source=poste_source,
  nb_IRIS=nb_IRIS,
  pourc_HLM=pourc_HLM,
  pourc_IMMIGRATION=pourc_IMMIGRATION,
  mean_ANNEE_EMMENAGEMENT=mean_ANNEE_EMMENAGEMENT,
  pourc_ASCENSEUR=pourc_ASCENSEUR,
  pourc_RESIDENCE_PRINCIPALE=pourc_RESIDENCE_PRINCIPALE,
  pourc_LOGEMENT_OCCASIONNEL=pourc_LOGEMENT_OCCASIONNEL,
  pourc_RESIDENCE_SECONDAIRE=pourc_RESIDENCE_SECONDAIRE,
  mean_AGE=mean_AGE,
  pourc_CHAUFFAGE_CENTRAL_COLLECTIF=pourc_CHAUFFAGE_CENTRAL_COLLECTIF,
  pourc_CHAUFFAGE_CENTRAL_INDIV=pourc_CHAUFFAGE_CENTRAL_INDIV,
  pourc_CHAUFFAGE_ELECTRIQUE=pourc_CHAUFFAGE_ELECTRIQUE,
  pourc_COMBUSTIBLE_ELECTRICITE=pourc_COMBUSTIBLE_ELECTRICITE,
  pourc_STATIONNEMENT=pourc_STATIONNEMENT,
  mean_NB_ELEVES_ETUDIANTS_14ANSPLUS=mean_NB_ELEVES_ETUDIANTS_14ANSPLUS,
  mean_NB_11ANSMOINS=mean_NB_11ANSMOINS,
  mean_NB_16ANSMOINS=mean_NB_16ANSMOINS,
  mean_NB_18ANSMOINS=mean_NB_18ANSMOINS,
  mean_NB_19ANSMOINS=mean_NB_19ANSMOINS,
  mean_NB_24ANSMOINS=mean_NB_24ANSMOINS,
  mean_NB_3ANSMOINS=mean_NB_3ANSMOINS,
  mean_NB_60ANSPLUS=mean_NB_60ANSPLUS,
  mean_NB_65ANSPLUS=mean_NB_65ANSPLUS,
  mean_NB_6ANSMOINS=mean_NB_6ANSMOINS,
  mean_NB_75ANSPLUS=mean_NB_75ANSPLUS,
  mean_NB_PERSONNES_ACTIVES=mean_NB_PERSONNES_ACTIVES,
  mean_NB_PERSONNES=mean_NB_PERSONNES,
  mean_NB_PERSONNES_MASCULIN=mean_NB_PERSONNES_MASCULIN,
  mean_NB_PERSONNES_FEMININ=mean_NB_PERSONNES_FEMININ,
  mean_NB_PERSONNES_ACTIVES_AVEC_EMPLOI=mean_NB_PERSONNES_ACTIVES_AVEC_EMPLOI,
  mean_NB_PERSONNES_SCOLARISEES=mean_NB_PERSONNES_SCOLARISEES,
  mean_NB_PIECES_LOGEMENT=mean_NB_PIECES_LOGEMENT,
  pourc_PROPRIETAIRE_LOGEMENT=pourc_PROPRIETAIRE_LOGEMENT,
  pourc_LOCATAIRE_LGMNT_VIDE_NON_HLM=pourc_LOCATAIRE_LGMNT_VIDE_NON_HLM,
  pourc_LOCATAIRE_LGMNT_VIDE_HLM=pourc_LOCATAIRE_LGMNT_VIDE_HLM,
  pourc_LOCATAIRE_MEUBLE=pourc_LOCATAIRE_MEUBLE,
  pourc_LOGE_GRATUITEMENT=pourc_LOGE_GRATUITEMENT,
  pourc_LOGMNT_MOINS_40M2=pourc_LOGMNT_MOINS_40M2,
  pourc_LOGMNT_40M2_A_100M2=pourc_LOGMNT_40M2_A_100M2,
  pourc_LOGMNT_PLUS_100M2=pourc_LOGMNT_PLUS_100M2,
  pourc_REFERENT_ACTIFS=pourc_REFERENT_ACTIFS,
  pourc_REFERENT_CHOMEUR=pourc_REFERENT_CHOMEUR,
  pourc_REFERENT_RETRAITE=pourc_REFERENT_RETRAITE,
  pourc_REFERENT_ETUDIANT=pourc_REFERENT_ETUDIANT,
  pourc_REFERENT_MOINS_14ANS=pourc_REFERENT_MOINS_14ANS,
  pourc_REFERENT_PERSONNE_AU_FOYER=pourc_REFERENT_PERSONNE_AU_FOYER,
  pourc_REFERENT_AUTRES_INACTIFS=pourc_REFERENT_AUTRES_INACTIFS,
  pourc_LOGEMENT_ISOLE=pourc_LOGEMENT_ISOLE,
  pourc_LOGEMENT_JUMELE=pourc_LOGEMENT_JUMELE,
  pourc_BATIMENT_2LOGEMENTS_ET_PLUS=pourc_BATIMENT_2LOGEMENTS_ET_PLUS,
  pourc_BATIMENT_NON_HABITATION=pourc_BATIMENT_NON_HABITATION,
  pourc_CONSTRUCTION_PROVISOIRE=pourc_CONSTRUCTION_PROVISOIRE,
  pourc_MAISONS=pourc_MAISONS,
  pourc_APPARTEMENTS=pourc_APPARTEMENTS,
  pourc_LOGEMENT_FOYER=pourc_LOGEMENT_FOYER,
  pourc_CHAMBRE_HOTEL=pourc_CHAMBRE_HOTEL,
  pourc_HABITATION_DE_FORTUNE=pourc_HABITATION_DE_FORTUNE,
  pourc_PIECE_INDEPENDANTE=pourc_PIECE_INDEPENDANTE
)

View(IndicateursINSEE)