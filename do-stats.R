source("init.R")

conso_HTA_xts=ConsoMeasParDepart[,HTA_names]

# Computing PS conso from HTA conso
for(PS_name in PS_names)
{
  consoPS=xts(
    rowSums(conso_HTA_xts[,stri_startswith_fixed(names(conso_HTA_xts), PS_name)]),
    time(conso_HTA_xts))
  conso_HTA_xts = merge(conso_HTA_xts, tmpname=xts(consoPS,time(conso_HTA_xts)), all = FALSE)
  names(conso_HTA_xts)[names(conso_HTA_xts) == "tmpname"] = PS_name
}

# Total number of HTA including PS
nb_HTA=length(All_names)

# Main dataframe to fill
Stats_conso=data.frame(HTA=All_names)


# Fills Stats_conso with yearly statistics
source(paste0(StatsSourceFolder,"do-yearstats.R"))

Stats_conso$Moyenne.annuelle = moy_annuel
Stats_conso$Variance.annuelle                 = 100 * var_annuel       / moy_annuel / moy_annuel
Stats_conso$Pic.annuel                        = 100 * conso_peak_an    / moy_annuel
Stats_conso$Pic.journalier.moyen              = 100 * peak_jour_moyen  / moy_annuel
Stats_conso$Creux.journalier.moyen            = 100 * nadir_jour_moyen / moy_annuel
Stats_conso$Ecart.journalier.moyen            = 100 * delta_jour_moyen / moy_annuel
Stats_conso$Ecart.Moyenne.Pic.journalier.moyen= 100 * delta_mean_peak  / moy_annuel
Stats_conso$Variance.journaliere.moyenne      = 100 * var_jour_moyen   / moy_annuel

#Stats_conso$Date.pic.annuel  = conso_peak_date
Stats_conso$Heure.annee.du.pic.annuel = as.numeric(
  as.POSIXct(conso_peak_date,origin="1970-01-01") -
    as.POSIXct(paste(year,"-01-01",sep="")),
  units="hours")



# Fills Stats_conso with thermosensibility statistics
source(paste0(StatsSourceFolder,"do-thermosens.R"))

Stats_conso$thermo_A      = 100 * thermo_A / moy_annuel
#Stats_conso$thermo_B      = 100 * thermo_B / moy_annuel
#Stats_conso$thermo_Erreur = thermo_Erreur
#Stats_conso$thermo_RSquare= thermo_RSquare


# Save statistics in a readable format
write.csv2(Stats_conso, paste0(StatsOutFolder,"Statistics.csv"))


# Plots graphs into pdf files
source(paste0(StatsSourceFolder,"do-plot.R"))

# Compute customer statistics
source(paste0(DataSourceFolder,"stats_par_offre.R"))

# Compute INSEE indicators

INSEEFileName = paste0(StatsOutFolder,"insee.Rdata")
if(file.exists(INSEEFileName))
{
  cat("Loading INSEE stats...\n")
  load(INSEEFileName)
} else {
  source("indicateurs_INSEE.R")
  save(IndicateursINSEE, file = INSEEFileName)
}

IndicateursINSEE = IndicateursINSEE[,
                                  !names(IndicateursINSEE) %in% c("pourc_REFERENT_MOINS_14ANS")
                                  ]


cat("Computing correlation matrices and plot them in pdf files.\n")
source("correlation.R")


cat("Do cross validation on several models.\n")
source(paste0(ModelSourceFolder,"main.R"))


cat("Do tertiaire function optimization.\n")
source(paste0(TertiaireSourceFolder,"main.R"))

