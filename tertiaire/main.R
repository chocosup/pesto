period="2011-07-01/2011-08-31"

C = ConsoMeasParDepart[period,HTA_names]
tIndices = time(C)

S = souscrit_par_offre[1:length(HTA_names),]

alpha = as.matrix(
  data.frame(
    alpha_residentiel = S$`bleu domestique` / 1000,
    alpha_agricole    = (S$`bleu agricole` + S$`jaune agricole`) / 1000,
    alpha_commercants = (S$`bleu artisans et commercants` + S$`jaune commercants`) / 1000,
    alpha_autres      = (S$Total - S$`bleu domestique` - S$`bleu agricole` - S$`jaune agricole` - S$`bleu artisans et commercants` - S$`jaune commercants`) / 1000
  )
)

beta = as.matrix(
  data.frame(
    beta_residentiel = S$`bleu domestique` / 1000,
    beta_autres      = (S$Total - S$`bleu domestique`) / 1000
  )
)











