#======================    Moyenne modulo n    =========================#
# Input :  X = tableau ou vecteur                                       #
# Output : Y = vecteur de longueur n. Y[i] est la moyenne des X[j]      #
#              prise sur l'ensemble des j congrus à i modulo n          #
#=======================================================================#


selectmod = function(X,n)
{
  X_ = as.numeric(X)
  l = ceiling(length(X)/n)
  Y = array(NA,l*n)
  Y[1:length(X_)] = X_
  return(rowMeans(matrix(Y,n,l), na.rm = TRUE))
}


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
# Output : liste (out, err_code)                                        #
#          out = X corrigé                                              #
#          err_code = code d'erreur : 0 si OK                           #
#=======================================================================#

correct_plg = function(X,lb,ub,j)
{
  out = X
  bds = c()
  cyc = vector(mode = "logical", 144)
  rsub = vector(mode = "numeric", 144)
  lsub = vector(mode = "numeric", 144)
  cnst = 0
  err_code = 0
  
  if (is.null(lb)) {
    # On tronque la plage de droite à j jours
    ub[2] = min(ub[2],ub[1]+144*j-1)
    # On définit la plage à remplacer
    bds = c(1, ub[1] - 1)
    if (ub[2] - ub[1] < 143) {
      # |_X
      
      # Méthode impossible à appliquer
      err_code = 1
      
    } else {
      # |_/
      
      # On prolonge juste le modèle de droite
      rsub = selectmod(X[ub[1]:ub[2]],144)
      rsub = rev(rep(rev(rsub), length.out = bds[2]-bds[1]+1))
      # Correction
      out[bds[1]:bds[2]] = rsub
      
    }
  } else if (is.null(ub)) {
    # On tronque la plage de gauche à j jours
    lb[1] = max(lb[2]-144*j+1,lb[1])
    # On définit la plage à remplacer
    bds = c(lb[2] + 1, length(X))
    if (lb[2] - lb[1] < 143) {
      # X_|
      
      # Méthode impossible à appliquer
      err_code = 2
    } else {
      # \_|
      
      # On prolonge juste le modèle de gauche
      lsub = rev(selectmod(rev(X[lb[1]:lb[2]]),144))
      lsub = rep(lsub, length.out = bds[2]-bds[1]+1)
      # Correction
      out[bds[1]:bds[2]] = lsub
      
    }
  } else {
    # On tronque les plages à droite et à gauche à j jours
    lb[1] = max(lb[2] - 144*j + 1, lb[1])
    ub[2] = min(ub[2] ,ub[1] + 144*j - 1)
    # On définit la plage à remplacer
    bds = c(lb[2] + 1, ub[1] - 1)
    if (lb[2] - lb[1] < 143) {
      if (ub[2] - ub[1] < 143) {
        # X_X
        
        # Méthode impossible à appliquer
        err_code = 3
      } else {
        # X_/
        
        # Pas de modèle à gauche, on utilise un offset affine
        c1 = mean(X[lb[1]:lb[2]])
        rsub = selectmod(X[ub[1]:ub[2]],144)
        c2 = mean(rsub)
        rsub = rsub - c2
        rsub = rev(rep(rev(rsub), length.out = bds[2]-bds[1]+1))
        # Création de l'offset
        offset = seq(c1,c2, length.out = bds[2] - bds[1] + 3)[-c(1, bds[2] - bds[1] + 3)]
        # Correction
        out[bds[1]:bds[2]] = offset + rsub
        
      }
    } else if (ub[2] - ub[1] < 143) {
      # \_X
      
      # Pas de modèle à droite, on utilise un offset affine
      c2 = mean(X[ub[1]:ub[2]])
      lsub = rev(selectmod(rev(X[lb[1]:lb[2]]),144))
      c1 = mean(lsub)
      lsub = lsub - c1
      lsub = rep(lsub, length.out = bds[2]-bds[1]+1)
      # Création de l'offset
      offset = seq(c1,c2, length.out = bds[2] - bds[1] + 3)[-c(1, bds[2] - bds[1] + 3)]
      # Correction
      out[bds[1]:bds[2]] = offset + lsub
      
    } else
    {
      # \_/
      
      # On fait les modèles journaliers à droite et à gauche
      rsub = selectmod(X[ub[1]:ub[2]],144)
      rsub = rev(rep(rev(rsub), length.out = bds[2]-bds[1]+1))
      lsub = rev(selectmod(rev(X[lb[1]:lb[2]]),144))
      lsub = rep(lsub, length.out = bds[2]-bds[1]+1)
      # On crée un vecteur pour les barycentres glissants
      poids = seq(0,1, length.out = ub[1] - lb[2] + 1)[-c(1, bds[2] - bds[1] + 3)]
      # On corrige la plage corrompue
      out[bds[1]:bds[2]] = (1-poids)*lsub + poids*rsub
    }
  }
  return(list(out,err_code))
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
  out = correct_plg(out,lbounds,ubounds,j)[[1]]
  
  # plages intermediaires
  if(ncol(M) >= 3) {
    for (i in 2:(ncol(M)-1))
    {
      lbounds = ubounds
      ubounds = c(M[2,i]+1, M[1,i+1]-1)
      out = correct_plg(out,lbounds,ubounds,j)[[1]]
    }
  }
  
  # derniere plage
  lbounds = ubounds
  if (length(X) == M[2,ncol(M)]) { ubounds = NULL} else { ubounds = c(M[2,ncol(M)]+1, length(X)) }
  out = correct_plg(out,lbounds,ubounds,j)[[1]]
  
  return(out)
}


