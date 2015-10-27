### Chargement des donnees clients ###
load(file = paste(ERDFDataFolder, separator, "Customers_", zonegeo, "_", year, "_PESTO.RData", sep = ""))

### Suppression des accents pour les libelles ###
levels(client_pesto_final$LIBELLE_ACTIVITE) = c("bleu agricole", "bleu artisans et commercants", "bleu domestique", "bleu equipement collectif", "bleu hotels restaurants", "jaune agricole", "jaune bureaux-enseignement-sante", "jaune commercants", "jaune hotels restaurants", "jaune industrie", "NON RENSEIGNE")

offres = levels(client_pesto_final$LIBELLE_ACTIVITE)
HTA_nb = length(HTA_names)

clients_par_offre = data.frame(CODE_GDO = HTA_names)
conso_par_offre = data.frame(CODE_GDO = HTA_names)



for(i in 1:length(offres))
{
  clients_par_offre[, offres[i]] = 0
  conso_par_offre[, offres[i]] = 0
}




for(i in 1:HTA_nb)
{
  temp = client_pesto_final[client_pesto_final$CODE_GDO == clients_par_offre$CODE_GDO[i], c("LIBELLE_ACTIVITE","Conso_customer_Wh")] # On isole les lignes et colonnes qui nous interessent. Isoler les lignes de cette facon semble etre beaucoup plus rapide que le faire manuellement en parcourant la liste, ça doit être vectoriel ou quelque chose comme ça...
  for(j in 1:length(offres))
  {
    clients_par_offre[i, offres[j]] = sum(temp$LIBELLE_ACTIVITE == offres[j])
    conso_par_offre[i, offres[j]] = sum(temp[temp$LIBELLE_ACTIVITE == offres[j], "Conso_customer_Wh"], na.rm = TRUE)
  }
}

###

compteur = HTA_nb + 1

for(PS_name in PS_names)
{
  # clients_par_offre
  clients_par_offre[compteur,-1] = colSums(clients_par_offre[stri_startswith_fixed(clients_par_offre$CODE_GDO[1:HTA_nb],PS_name),-1])
  levels(clients_par_offre$CODE_GDO) = append(levels(clients_par_offre$CODE_GDO), PS_name)
  clients_par_offre[compteur,1] = PS_name
  
  # conso_par_offre
  conso_par_offre[compteur,-1] = colSums(conso_par_offre[stri_startswith_fixed(conso_par_offre$CODE_GDO[1:HTA_nb],PS_name),-1])
  levels(conso_par_offre$CODE_GDO) = append(levels(conso_par_offre$CODE_GDO), PS_name)
  conso_par_offre[compteur,1] = PS_name
  
  compteur = compteur + 1
}

###

clients_par_offre[, "Total"] = rowSums(clients_par_offre[, offres])
conso_par_offre[, "Total"] = rowSums(conso_par_offre[, offres])

rm(temp)

moy_par_offre = conso_par_offre[, -ncol(conso_par_offre)]
part_par_offre = conso_par_offre[, -ncol(conso_par_offre)]

for(i in offres[-length(offres)])
{
  moy_par_offre[,i] = conso_par_offre[,i]/clients_par_offre[,i]
  part_par_offre[,i] = conso_par_offre[,i]/conso_par_offre$Total
}

###

write.csv2(clients_par_offre,  file = paste0(DataOutFolder, "clients_par_offre.csv"))
write.csv2(conso_par_offre,    file = paste0(DataOutFolder, "conso_par_offre.csv"))
write.csv2(moy_par_offre,      file = paste0(DataOutFolder, "moy_par_offre.csv"))
write.csv2(part_par_offre,     file = paste0(DataOutFolder, "part_par_offre.csv"))


########## debug ############

pb = c()

for(i in 1:HTA_nb)
{
  for(j in offres[-length(offres)])
  {
    if(conso_par_offre[i,j] == 0 && clients_par_offre[i,j] != 0)
    {
      pb = append(pb, as.character(j))
    }
  }
}