#======================    Blocs de TRUE    ========================#
# Input : tableau X de booléens                                     #
# Output : matrice M = pour chaque bloc de TRUE consécutifs de x,   #
#             une colonne avec l'indice de début et de fin du bloc  #
#===================================================================#

blocs_true <- function(X)
{
  x = as.logical(X)
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


#===============    Plus grandes plages d'erreurs    ===================#
# Input : X = matrice, xts ou data.frame                                #
#         (censé représenter la conso NORMALISEE d'un départ)           #
#         threshold = seuil à partir duquel un point est aberrant       #
# Output : out = vecteur de longueur ncol(X) avec les plus grandes      #
#                plages de données aberrantes                           #
#=======================================================================#

consecutives = function(X, threshold)
{
  out = c()
  for (i in 1:ncol(X))
  {
    mat = blocs_true(as.logical(abs(X[,i]) > threshold))
    if (!is.null(mat)) {out = append(out, max(mat[2,] - mat[1,] + 1))}
    else {out = append(out,0)}
  }
  return(out)
}


#=======================    Correct plage    ===========================#
# Input :  X = vecteur (censé représenter la conso d'un départ)         #
#          lb = vect de taille 2, données valides avant la plage        #
#          ub = vect de taille 2, données valides après la plage        #
#          j = jours max à moyenner à droite et à gauche d'un trou      #
# Output : out = X corrigé                                              #
#=======================================================================#

correct_plg = function(X,lb,ub,j)
{
  out = X
  cyc = vector(mode = "logical", 144)
  
  if (is.null(lb)) {
    ub[2] = min(ub[2],ub[1]+144*j-1)
    if (ub[2] - ub[1] < 143) {
      # |_X ???
    } else {
      # |_/
    }
  } else if (is.null(ub)) {
    lb[1] = max(lb[2]-144*j+1,lb[1])
    if (lb[2] - lb[1] < 143) {
      # X_| ???
    } else {
      # \_|
    }
  } else {
    lb[1] = max(lb[2]-144*j+1,lb[1])
    ub[2] = min(ub[2],ub[1]+144*j-1)
    if (lb[2] - lb[1] < 143) {
      if (ub[2] - ub[1] < 143) {
        # X_X ???
      } else {
        # X_/
      }
    } else if (ub[2] - ub[1] < 143) {
      # \_X
    } else
    {
      # \_/
    }
  }
  return(out)
}


#=========================    CORRECT    ===============================#
# Input :  X = vecteur (censé représenter la conso d'un départ)         #
#          M = matrice de plages à corriger                             #
#          j = jours max à moyenner à droite et à gauche d'un trou      #
# Output : out = X corrigé                                              #
#                 !!!   DEPEND DE correct_plg   !!!                     #
#=======================================================================#

correct = function(X,M,j)
{
  if (is.null(M)) {return(X)}
  out = X
  
  # premiere plage
  if (M[1,1] == 1) { lbounds = NULL } else { lbounds = c(1,M[1,1]-1) }
  ubounds = c(M[2,1] + 1, M[1,2] - 1)
  out = correct_plg(out,lbounds,ubounds,j)
  
  # plages intermediaires
  for (i in 2:(ncol(M)-1))
  {
    lbounds = ubounds
    ubounds = c(M[2,i]+1, M[1,i+1]-1)
    out = correct_plg(out,lbounds,ubounds,j)
  }
  
  # derniere plage
  lbounds = ubounds
  if (length(X) == M[2,ncol(M)]) { ubounds = NULL} else { ubounds = c(M[2,ncol(M)]+1, length(X)) }
  out = correct_plg(out,lbounds,ubounds,j)
  
  return(out)
}