
# Rendu visuel simplifie de la matrice de correlation
myImagePlot <- function(x, ...){
  min <- min(x)
  max <- max(x)
  yLabels <- rownames(x)
  yLabels2 <- rownames(x)
  xLabels <- colnames(x)
  xLabels2<- colnames(x)
  title <-c()
  # Verification pour ne pas faire planter le programme
  if( length(list(...)) ){
    Lst <- list(...)
    if( !is.null(Lst$zlim) ){
      min <- Lst$zlim[1]
      max <- Lst$zlim[2]
    }
    if( !is.null(Lst$yLabels) ){
      yLabels2 <- c(Lst$yLabels)
    }
    if( !is.null(Lst$xLabels) ){
      xLabels2 <- c(Lst$xLabels)
    }
    if( !is.null(Lst$title) ){
      title <- Lst$title
    }
  }
  # Pour enlever les valeurs nulles
  if( is.null(xLabels) ){
    xLabels <- c(1:ncol(x))
  }
  if( is.null(yLabels) ){
    yLabels <- c(1:nrow(x))
  }
  
  layout(matrix(data=c(1,3,2,2), nrow=2, ncol=2), widths=c(15,1), heights=c(2,1))
  
  # Definition des niveaux de couleurs
  ColorRamp <- rgb( seq(0,1,length=256),  # Red
                    seq(0,1,length=256),  # Green
                    seq(1,0,length=256))  # Blue
  ColorLevels <- seq(min, max, length=length(ColorRamp))
  
  # Inverser l'axe des X
  reverse <- nrow(x) : 1
  yLabels <- yLabels[reverse]
  x <- x[reverse,]
  
  # Carte graphique
  par(mar = c(2,2,2,1))
  image(1:length(xLabels), 1:length(yLabels), t(x), col=ColorRamp, xlab="",
        ylab="", axes=FALSE, zlim=c(min,max))
  if( !is.null(title) ){
    title(main=title)
  }
  axis(BELOW<-1, at=1:length(xLabels), labels=xLabels, cex.axis=0.7)
  axis(LEFT <-2, at=1:length(yLabels), labels=yLabels, las= HORIZONTAL<-1,
       cex.axis=0.7)
  
  # L'echelle de teintes
  
  par(mar = c(1,1,1,1))
  image(1, ColorLevels,
        matrix(data=ColorLevels, ncol=length(ColorLevels),nrow=1),
        col=ColorRamp,
        xlab="",ylab="",
        xaxt="n")
  
  
  par(mar = c(0,0,0,0))
  plot.new()
  text(0.25,0.95,"Axe X")
  for (i in 1:length(xLabels2)) {
    text(0.25,
         0.9 - 0.8 * (i / length(xLabels2)),
         paste(i, ":", xLabels2[i]), cex=0.8)
  }
  
  text(0.75,0.95,"Axe Y")
  for (i in 1:length(yLabels2)) {
    text(0.75,
         0.9 - 0.8 * (i / length(yLabels2)),
         paste(i, ":", yLabels2[i]), cex=0.8)
  }
  
  layout(1)
}


openPDF(paste0(StatsOutFolder,"Correlations_",year))



# Travail preparatoire : obtention d'une matrice plutot qu'un data frame 
D<-as.matrix(clients_par_offre)
D<-D[,-1]

D1<-as.numeric( str_replace(D,",",".") )
Ltemp<-matrix(D1,ncol=ncol(D))
inds <- 1:ncol(D) # pour limiter l'etude si necessaire
La<-Ltemp[,inds]

# Obtention de la matrice de la correlation
correlat<-abs(cor(La,use="complete.obs"))

# Representation de la matrice
Visuel<-myImagePlot(correlat,
                    xLabels=names(clients_par_offre)[-1],
                    yLabels=names(clients_par_offre)[-1],
                    title="Correlation stats population")




# Travail preparatoire : obtention d'une matrice plutot qu'un data frame 
D<-as.matrix(Stats_conso)
D<-D[,-1]

D1<-as.numeric(str_replace(D,",",".") )
Ltemp<-matrix(D1,ncol=ncol(D))
inds <- 1:ncol(D) #pour limiter l'etude si necessaire
Lb<-Ltemp[,inds]

# Obtention de la matrice de la correlation
correlat<-abs(cor(Lb,use="complete.obs"))

# Representation de la matrice
Visuel<-myImagePlot(correlat,
                    xLabels=names(Stats_conso)[-1],
                    yLabels=names(Stats_conso)[-1],
                    title="Correlation stats conso")



# Obtention de la matrice de la correlation
correlat<-abs(cor(La, Lb, use="complete.obs"))

# Representation de la matrice
Visuel<-myImagePlot(correlat,
                    xLabels=names(Stats_conso)[-1],
                    yLabels=names(clients_par_offre)[-1],
                    title="Correlation population / consommation")



# Travail preparatoire : obtention d'une matrice plutot qu'un data frame 
D<-as.matrix(IndicateursINSEE)
D<-D[,-1]
D<-D[,-1]

D1<-as.numeric( str_replace(D,",",".") )
Ltemp<-matrix(D1,ncol=ncol(D))
inds <- 1:ncol(D) # pour limiter l'etude si necessaire
La<-Ltemp[,inds]

# Obtention de la matrice de la correlation
correlat<-abs(cor(La,use="complete.obs"))

# Representation de la matrice
Visuel<-myImagePlot(correlat,
                    xLabels=names(IndicateursINSEE)[-1][-1],
                    yLabels=names(IndicateursINSEE)[-1][-1],
                    title="Correlation stats Sylvain")



# Obtention de la matrice de la correlation
correlat<-abs(cor(La, Lb, use="complete.obs"))

# Representation de la matrice
Visuel<-myImagePlot(correlat,
                    xLabels=names(Stats_conso)[-1],
                    yLabels=names(IndicateursINSEE)[-1][-1],
                    title="Correlation population / Sylvain")

closePDF()
