
a <- as.matrix(souscrit_par_offre["bleu domestique"])
b <- a
c <- as.matrix(souscrit_par_offre["Total"]) - a


period="2011-07-01/2011-08-31"


C = ConsoMeasParDepart[period,HTA_names]
tIndices = time(C)















