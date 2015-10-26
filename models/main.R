

# TODO: remove redondant / useless columns
X=cbind(as.matrix(IndicateursINSEE[-2][-1][1:length(HTA_names),]),
        as.matrix(part_par_offre[-1][1:length(HTA_names),]),
        as.matrix(clients_par_offre[1:length(HTA_names),"Total"]),
        as.matrix(conso_par_offre[1:length(HTA_names),"Total"]))
colnames(X)[ncol(X)-1]="Clients"
colnames(X)[ncol(X)]  ="Conso"


nobs = nrow(X)
sampleSize = nobs



nbAlgos = 4
nbVariables = 3

resultsLeaveOneOut = matrix(NA,nbAlgos,nbVariables)
rownames(resultsLeaveOneOut) <- array(NA,nbAlgos)
colnames(resultsLeaveOneOut) <- c("Moyenne annuelle",
                                  "Variance annuelle",
                                 "Heure pic annuel")


cat("\n-----------------------\n")
cat("   Moyenne annuelle\n")
cat("-----------------------\n")
modelized_quantity = "annual mean"
Y=as.matrix(Stats_conso$Moyenne.annuelle[1:length(HTA_names)])

source(paste0(ModelSourceFolder,"simple_linear_model.R"))
rownames(resultsLeaveOneOut)[1] = modelName
modelName = paste0(modelName, " for ", modelized_quantity)
source(paste0(ModelSourceFolder,"leaveoneout.R"))
resultsLeaveOneOut[1,1] = rmse

source(paste0(ModelSourceFolder,"lars_model.R"))
rownames(resultsLeaveOneOut)[2] = modelName
modelName = paste0(modelName, " for ", modelized_quantity)
source(paste0(ModelSourceFolder,"leaveoneout.R"))
resultsLeaveOneOut[2,1] = rmse

source(paste0(ModelSourceFolder,"NN_model.R"))
rownames(resultsLeaveOneOut)[3] = modelName
modelName = paste0(modelName, " for ", modelized_quantity)
source(paste0(ModelSourceFolder,"leaveoneout.R"))
resultsLeaveOneOut[3,1] = rmse

source(paste0(ModelSourceFolder,"random_forest_model.R"))
rownames(resultsLeaveOneOut)[4] = modelName
modelName = paste0(modelName, " for ", modelized_quantity)
source(paste0(ModelSourceFolder,"leaveoneout.R"))
resultsLeaveOneOut[4,1] = rmse



cat("\n-----------------------\n")
cat("   Variance annuelle\n")
cat("-----------------------\n")
modelized_quantity = "annual variance"
Y=as.matrix(Stats_conso$Variance.annuelle[1:length(HTA_names)])

source(paste0(ModelSourceFolder,"simple_linear_model.R"))
modelName = paste0(modelName, " for ", modelized_quantity)
source(paste0(ModelSourceFolder,"leaveoneout.R"))
resultsLeaveOneOut[1,2] = rmse

source(paste0(ModelSourceFolder,"lars_model.R"))
modelName = paste0(modelName, " for ", modelized_quantity)
source(paste0(ModelSourceFolder,"leaveoneout.R"))
resultsLeaveOneOut[2,2] = rmse

source(paste0(ModelSourceFolder,"NN_model.R"))
modelName = paste0(modelName, " for ", modelized_quantity)
source(paste0(ModelSourceFolder,"leaveoneout.R"))
resultsLeaveOneOut[3,2] = rmse

source(paste0(ModelSourceFolder,"random_forest_model.R"))
modelName = paste0(modelName, " for ", modelized_quantity)
source(paste0(ModelSourceFolder,"leaveoneout.R"))
resultsLeaveOneOut[4,2] = rmse



cat("\n-----------------------\n")
cat("   Heure pic annuel\n")
cat("-----------------------\n")
modelized_quantity = "annual peak hour"
Y=as.matrix(Stats_conso$Heure.annee.du.pic.annuel[1:length(HTA_names)])

source(paste0(ModelSourceFolder,"simple_linear_model.R"))
modelName = paste0(modelName, " for ", modelized_quantity)
source(paste0(ModelSourceFolder,"leaveoneout.R"))
resultsLeaveOneOut[1,3] = rmse

source(paste0(ModelSourceFolder,"lars_model.R"))
modelName = paste0(modelName, " for ", modelized_quantity)
source(paste0(ModelSourceFolder,"leaveoneout.R"))
resultsLeaveOneOut[2,3] = rmse

source(paste0(ModelSourceFolder,"NN_model.R"))
modelName = paste0(modelName, " for ", modelized_quantity)
source(paste0(ModelSourceFolder,"leaveoneout.R"))
resultsLeaveOneOut[3,3] = rmse

source(paste0(ModelSourceFolder,"random_forest_model.R"))
modelName = paste0(modelName, " for ", modelized_quantity)
source(paste0(ModelSourceFolder,"leaveoneout.R"))
resultsLeaveOneOut[4,2] = rmse