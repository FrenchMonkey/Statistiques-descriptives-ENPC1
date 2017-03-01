############################################
## fonction pour l'acp centree reduite
############################################

myAcpRed <- function(data, graph = FALSE){

      
      #############################################
      ##Transformation des donnÃ©es et matrice de Variance
      #############################################
      
      dataMatrix <- as.matrix(data)
      cenRed <- function(x){y <- sqrt((1/nrow(dataMatrix))*sum((x - mean(x))^2))
                      out <- (x - mean(x))/y
                      return(out)}
      dataNew <- apply(dataMatrix, 2, cenRed)
      V <- (1/nrow(dataNew))*(t(dataNew)%*%dataNew)

      ###########################################
      ## recherche des composantes principales
      ###########################################

      diagoV <- eigen(V)

      composantePrincipal <- apply(diagoV[[2]],2, function(x){out <- dataNew%*%x
                                          return(out)})
      valeurPrincipal <- diagoV[[1]]
      inertieTotale <- sum(valeurPrincipal)
      Pourcentage_inertie <- valeurPrincipal/inertieTotale

      #########################################################################################################

      ###########################################
      ##Illustration avec les deux axes principaux (PC1 et PC2)
      ###########################################

      correlations <- matrix(c(sqrt(valeurPrincipal[1])*diagoV[[2]][,1], sqrt(valeurPrincipal[2])*diagoV[[2]][,2]), ncol = 2)

      if(graph){

           ############################################
           ##cercles des corrÃ©lations
           ############################################

           x11()
           u <- seq(0,2*pi,0.001)
           plot(cos(u), sin(u), type = "l", main = "cercle des corrÃ©lations pour le premier plan factoriel", xlab = "PC1", ylab = "PC2")
           polygon(c(0,0),c(-1,1),  lty = 3)
           polygon(c(-1,1), c(0,0), lty = 3)

           aux1 <- sapply(1:ncol(dataNew), function(int){arrows(0,0,correlations[int,1],correlations[int,2], angle = 15, length = 0.10)
                                   text(correlations[int, 1],correlations[int,2], labels = colnames(dataNew)[int])})

          ###############################################
          ##Projection sur le premier plan factoriel
          ###############################################

          x11()
          xx <- c(min(composantePrincipal[,1]), max(composantePrincipal[,1]))
          yy <- c(min(composantePrincipal[,2]), max(composantePrincipal[,2]))


          plot(composantePrincipal, type = "p", pch = 16, xlab  = "PC1", ylab = "PC2", 
                                                                      main = "Projections des individus sur le premier plan factoriel")
          polygon(c(0,0), c(yy[1],yy[2]), lty = 3)
          polygon(c(xx[1],xx[2]), c(0,0), lty = 3)

          aux2 <- sapply(1:nrow(dataNew), function(int){text(composantePrincipal[int,1], composantePrincipal[int,2]-0.15, labels = int)})
 

         x11()
         xx <- c(min(composantePrincipal[,1]), max(composantePrincipal[,1]))
         yy <- c(min(composantePrincipal[,2]), max(composantePrincipal[,2]))


         plot(composantePrincipal, type = "p", pch = 16, xlab  = "PC1", ylab = "PC2", main = "RÃ©sumÃ© sur le premier plan factoriel")
         polygon(c(0,0), c(yy[1],yy[2]), lty = 3)
         polygon(c(xx[1],xx[2]), c(0,0), lty = 3)

         u <- seq(0,2*pi,0.001)
         lines(cos(u), sin(u))


         aux2 <- sapply(1:nrow(dataNew), function(int){text(composantePrincipal[int,1], composantePrincipal[int,2]-0.15, labels = int)})
         aux1 <- sapply(1:ncol(dataNew), function(int){arrows(0,0,correlations[int,1],correlations[int,2], angle = 15, length = 0.10)
                                   text(correlations[int, 1],correlations[int,2], labels = colnames(dataNew)[int])})



    


       }

     return(list(composantePrincipal = composantePrincipal, valeurPrincipal = valeurPrincipal, 
                                   Pourcentage_inertie = Pourcentage_inertie, inertieTotale = inertieTotale, correlations = correlations))
}

#debut du tp
data(iris)
#acp_iris<-myAcpRed(iris[,1:4,],graph=T)#manque un package
acp_iris<-prcomp(iris[,1:4,])
plot(acp_iris)
names(acp_iris)
#donne les coordonnées des variables explicatives de l'acp en fonction des variables de base
acp_iris[2]