# TODO: remove redondant / useless columns
X=cbind(as.matrix(IndicateursINSEE[-2][-1][1:length(HTA_names),]),
        as.matrix(part_par_offre[-1][1:length(HTA_names),]),
        as.matrix(clients_par_offre[1:length(HTA_names),"Total"]),
        as.matrix(conso_par_offre[1:length(HTA_names),"Total"]))
colnames(X)[ncol(X)-1]="Clients"
colnames(X)[ncol(X)]  ="Conso"

nobs = nrow(X)

#----------------------------------
#    Change below to configure
#----------------------------------

sampleSize = nobs

runAlgo=data.frame(
  Simple.linear=TRUE,
  LARS=TRUE,
  Random.Forest=TRUE,
  NN=TRUE
)

runVar=data.frame(
  Moyenne.annuelle=TRUE,
  Variance.annuelle=TRUE,
  Pic.annuel=TRUE,
  Pic.journalier.moyen=TRUE,
  Heure.annee.du.pic.annuel=TRUE,
  thermo_A=TRUE
)

#----------------------------------


algoNames=names(runAlgo)[which(runAlgo==TRUE)]
nbAlgos=length(algoNames)

varNames=names(runVar)[which(runVar==TRUE)]
nbVariables=length(varNames)

resultsLeaveOneOut = matrix(NA,nbAlgos,nbVariables)
rownames(resultsLeaveOneOut) <- algoNames
colnames(resultsLeaveOneOut) <- varNames




for (variableName in varNames)
{
  cat("\n-----------------------\n")
  cat("   ",variableName,"\n")
  cat("-----------------------\n")
  
  Y=as.matrix(Stats_conso[,variableName][1:length(HTA_names)])
  
  pdf(paste0(StatsOutFolder,"models_",variableName,"_",year,".pdf"))
  
  
  for (algoName in algoNames)
  {
    modelName = paste0(algoName, " model for ", variableName)
    cat("Doing: ",modelName,"\n")
    
    if (algoName == "Simple.linear") {
      source(paste0(ModelSourceFolder,"simple_linear_model.R"))
    } else if (algoName == "LARS") {
      source(paste0(ModelSourceFolder,"lars_model.R"))
    } else if (algoName == "NN") {
      source(paste0(ModelSourceFolder,"NN_model.R"))
    } else if (algoName == "Random.Forest") {
      source(paste0(ModelSourceFolder,"random_forest_model.R"))
    }
    
    source(paste0(ModelSourceFolder,"leaveoneout.R"))
    resultsLeaveOneOut[algoName,variableName] = rmse
  }
  
  dev.off()
}

