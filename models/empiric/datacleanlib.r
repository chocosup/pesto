#======================    Blocs de TRUE    ========================#
# Input : tableau x de booléens                                     #
# Output : matrice M = pour chaque bloc de TRUE consécutifs de x,   #
#             une colonne avec l'indice de début et de fin du bloc  #
#===================================================================#

blocs_true <- function(x)
{
  M = NULL
  b = FALSE
  a = NULL
  ncol = 0
  for(i in 1:length(x))
  {
    a = x[i]
    if (a & !b) {
      M = cbind(M, c(i,0))
      ncol = ncol + 1
    } else if (b & !a) {
      M[2,ncol] = i-1
    }
    b = a
  }
  if(a) { M[2,ncol] = length(x)}
  return(M)
}


#========    Plus petite "plage pertinente de données"    ==========#
# Input : Matrice M de dim (2,n)                                    #
#         (censée représenter les plages de "trous")                #
# Output : vecteur (indice, longueur) de la plus courte plage       #
#===================================================================#

min_plage <- function(M)
{
  # années bissextiles
  points = 0
  if(year %% 4) {points = 144 * 365}
  else {points = 144 * 366}
  
  ncol = length(M[1,])
  v = c(M[1,1] - 1)
  for (i in 2:ncol) {
    v = append(v, M[1,i] - M[2,i-1] - 1)
  }
  v = append(v, points - M[2,ncol])
  return(c(which.min(v),min(v)))
}

#============    "plages pertinentes de données" trop petites   ============#
# Inputs : M = Matrice de dim (2,n) censée représenter les plages de trous  #
#         (censée représenter les plages de trous)                          #
#          n = nombre de jours minimal d'une "plage pertinente" acceptable  #
# Output : P = Matrice dont les colonnes sont des couples                   #
#              (numéro de la plage, longueur) des plages trop petites.      #
#              !!! renvoie NULL si toutes les plages sont OK !!!            #
#===========================================================================#

small_ranges <- function(M,n)
{
  # années bissextiles
  points = 0
  if(year %% 4) {points = 144 * 365}
  else {points = 144 * 366}
  
  acceptable = n*144
  
  ncol = length(M[1,])
  P = NULL
  a = M[1,1] - 1
  
  if (a < acceptable) { P = cbind(P, c(0, a)) }
  
  for (i in 1:(ncol-1))
  {
    a = M[1,i+1] - M[2,i] - 1
    if (a < acceptable) { P = cbind(P, c(i, a)) }
  }
  
  a = points - M[2,ncol]
  if (a < acceptable) { P = cbind(P, c(ncol, a)) }
  
  return(P)
}


#======================    Normalisation    ========================#
# Input : X = matrice ou XTS, n = nombre de jours sur lesquels      #
#         la moyenne est calculée, b = normalisé par sd ? (booléen) #
# Output : R = X normalisé                                          #
#===================================================================#

normalise <- function(X,n,b)
{
  d = n*144
  lx = length(X[,1])
  R = X
  nd = lx %/% d
  for (i in 1:nd)
  {
    R[((i-1)*d + 1):(i*d),] = scale(R[((i-1)*d + 1):(i*d),], scale = b)
  }
  R[(nd*d):lx,] = scale(R[(nd*d):lx,], scale = b)
  
  return(R)
}


#=====================    Moyenne par heure    =========================#
# Input : X = vecteur (censé représenter la conso d'un départ)          #
# Output : out = vecteur de longueur 144 moyennant la conso par heure   #
#=======================================================================#

moyparheure = function(X)
{
  cyc = vector(mode = "logical", 144)
  out = vector(mode = "numeric", 144)
  for (i in 1:144)
  {
    cyc[] = FALSE
    cyc[i] = TRUE
    out[i] = mean(as.numeric(X)[cyc])
  }
  return(out)
}