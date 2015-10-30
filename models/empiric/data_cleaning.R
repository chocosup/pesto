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

