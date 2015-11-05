source("init.R")
source(paste0(EmpiricSourceFolder, "cleanlib.R"))

### Restreidre aux départs HTA étudiés ###
NewConsoMeas = ConsoMeasParDepart[,HTA_names]

### Calcul du pourcentage de zéros par départ ###
pourcentages_zero = data.frame()

for(HTA_name in HTA_names)
{
  pourcentages_zero[1,HTA_name] = sum(NewConsoMeas[, HTA_name] == 0) / length(NewConsoMeas[,HTA_name])
}

### Virer les départs avec plus de 5 % de mesures à 0 ###
HTA_ok = as.character(colnames(pourcentages_zero[1, pourcentages_zero < 0.05]))
NewConsoMeas = ConsoMeasParDepart[,HTA_ok]
pourcentages_zero = pourcentages_zero[,HTA_ok]

### Enlever les departs producteurs ###
mins = data.frame()
for(HTA_name in HTA_ok)
{
  mins[1,HTA_name] = min(NewConsoMeas[,HTA_name])
}
HTA_ok = as.character(colnames(mins[1, mins >= 0]))
NewConsoMeas = ConsoMeasParDepart[,HTA_ok]
pourcentages_zero = pourcentages_zero[,HTA_ok]

### HEURISTIQUE DE CORRECTION DES DONNÉES ###
# heuristique = function(X)
# {
#   Y = normalise(X,1,TRUE)
#   Y = Y[abs(Y) > 4]
#   Y[is.na(Y)] <- FALSE
#   return(Y)
# }

# CorData = correct_framework(NewConsoMeas, 1:ncol(NewConsoMeas), heuristique, 5)
