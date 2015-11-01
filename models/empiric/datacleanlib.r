#================    Longueur maximale de TRUE    ==================#
# Input : tableau x de booléens                                     #
# Output : entier b = longueur maximale de TRUE consécutifs dans x  #
#===================================================================#

consecutive <- function(x)
{
  a = 0
  b = 0
  for(i in 1:length(x))
  {
    if(x[i])
    {
      a = a+1
      if(a > b){b = a}
    }
    else{a = 0}
  }
  return(b)
}


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
# Output : vecteur (indice, longueur) de la plus longue plage       #
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