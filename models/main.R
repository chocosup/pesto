# TODO: remove redondant / useless columns
X=cbind(as.matrix(IndicateursINSEE[-2][-1][1:length(HTA_names),]),
        as.matrix(part_par_offre[-1][1:length(HTA_names),]),
        as.matrix(clients_par_offre[1:length(HTA_names),"Total"]),
        as.matrix(conso_par_offre[1:length(HTA_names),"Total"]))
colnames(X)[ncol(X)-1]="Clients"
colnames(X)[ncol(X)]  ="Conso"

nobs = nrow(X)

source(paste0(ModelSourceFolder,"config.R"))

algoFiles=data.frame(
  Mean          = "mean_model.R",
  Thibaud       = "thibaud_model.R",
  Simple.linear = "simple_linear_model.R",
  LARS          = "lars_model.R",
  Simple.LARS   = "simple_lars_model.R",
  Random.Forest = "random_forest_model.R",
  NN            = "NN_model.R",
  gunnar        = "gunnar.R"
)


algoNames=names(runAlgo)[which(runAlgo==TRUE)]
nbAlgos=length(algoNames)

varNames=names(runVar)[which(runVar==TRUE)]
nbVariables=length(varNames)

resultsLeaveOneOut = matrix(NA,nbAlgos,nbVariables)
rownames(resultsLeaveOneOut) <- algoNames
colnames(resultsLeaveOneOut) <- varNames

timeLeaveOneOut = matrix(NA,nbAlgos,nbVariables)
rownames(timeLeaveOneOut) <- algoNames
colnames(timeLeaveOneOut) <- varNames



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
    
    t0 = Sys.time()
    source(paste0(ModelSourceFolder,as.character(algoFiles[[algoName]])))
    source(paste0(ModelSourceFolder,"leaveoneout.R"))
    
    resultsLeaveOneOut[algoName,variableName] = rmse
    timeLeaveOneOut[algoName,variableName] = as.numeric(Sys.time() - t0,units="secs")
    cat("Duration: ",format(Sys.time() - t0),"\n")
  }
  
  while (as.numeric(dev.cur()) > 1) {
    dev.off()
  }
}

