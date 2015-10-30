rm(list=ls()); gc();  # Efface tout et libere la RAM

year=2012       #  2012
zonegeo="Bretagne"  #  Lyon
separator="/"   #  \\

cat("Initialization.\n")
cat("Zone:  ", zonegeo,"\n")
cat("Annee: ", year,"\n")

# HTA departs blacklist
HTA_blacklist=c("LOSCOC0121","LOSCOC0126","LOSCOC0156","COATAC0013","COATAC0012","COATAC0013","SSHERC0002",
                "CARHAC0018","SSRENC0003","SSRENC0004","SSRENC0006","SSRENC0009","SSRENC0011","ABERSC0006",
                "COATAC0009",       # Unsure
                "A.LIGC0200","A.LIGC2600","A.LIGC0600","A.LIGC1313","A.LIGC1800","A.LIGC1200","A.LIGC1700",
                "A.LIGC2512","B.ETOC1202","B.ETOC2664","B.ETOC0700","B.ETOC2121","B.ETOC1000","B.ETOC1300",
                "BONNEC4700","BONNEC3600","BONNEC3400","BONNEC1800","CUSS2C2804","MOUCHC1363"
)

library(lars)
library(sp)
library(zoo)
library(maptools)
library(xts)
library(Rcpp)
library(stringr)
require(sp)
library(stringi)
library(permute)

Sys.setenv(TZ='UTC')

# Main folders
DataFolder =paste0(getwd(), separator,  "data" , separator)
OuputFolder=paste0(getwd(), separator, "output", separator)

ERDFDataFolder  =paste0(getwd(), separator, "data", separator, "data_pesto_29_69_ERDF"    , separator)
RegionDataFolder=paste0(getwd(), separator, "data", separator, "data_pesto_regions_france", separator)
INSEEDataFolder =paste0(getwd(), separator, "data", separator, "INSEE"                    , separator)
MeteoDataFolder =paste0(getwd(), separator, "data", separator, "meteo"                    , separator)

# HTA consumption file
MesureFileName=paste0(ERDFDataFolder,"Mesures",separator,"ConsoMeasParDepart_",zonegeo,year, ".xts.RData")
# HTA reorga file
ReorgaFileName=paste0(ERDFDataFolder,"Mesures",separator,"ReorgaParDepart_"   ,zonegeo,year, ".xts.RData")
# Client data file
ClientFileName=paste0(ERDFDataFolder,"Customers_",zonegeo,"_",year,"_PESTO.RData")
# Meteo file
MeteoFileName=paste0(MeteoDataFolder,"Temperature.xts.", zonegeo, "_1997-2014.RData")


#--------------------------------------------------------------
#       Dossiers du groupe "stats" (Thibaud, Gaspard)
#--------------------------------------------------------------

# output
StatsOutFolder = paste0(OuputFolder, "stat_", zonegeo, "_", year, separator)
if(!file.exists(StatsOutFolder))
{
  dir.create(StatsOutFolder)
}
# sources
StatsSourceFolder=paste0(getwd(),separator,"stats",separator)



#--------------------------------------------------------------
#       Dossiers du groupe "data" (Sylvain, Ambroise)
#--------------------------------------------------------------

# output
DataOutFolder = paste0(OuputFolder, "data_", zonegeo, "_", year, separator)
if(!file.exists(DataOutFolder))
{
  dir.create(DataOutFolder)
}

# sources
DataSourceFolder=paste0(getwd(),separator,"data_team_sources",separator)


#--------------------------------------------------------------
#                 Dossier de modelisation
#--------------------------------------------------------------
ModelSourceFolder=paste0(getwd(),separator,"models",separator,"machine learning", separator)




# Load customer base to retrieve the list of interesting HTA
load(file=ClientFileName)
HTA_names<-unique(client_pesto_final$CODE_GDO)
HTA_names<-HTA_names[!(HTA_names %in% HTA_blacklist)]
HTA_names=as.character(HTA_names)

# Load measures from the interesting HTA only
load(file=MesureFileName)
HTA_names<-HTA_names[HTA_names %in% names(ConsoMeasParDepart)]

# HTA names
HTA_names=HTA_names[order(HTA_names)]
cat("HTA found:", length(HTA_names),"\n")

# PS names
PS_names=unique(substr(HTA_names,1,5))
PS_names=PS_names[order(PS_names)]
cat("PS found:", length(PS_names),"\n")

# All names HTA + PS
All_names=c(HTA_names, PS_names)


