######## Mise en forme tableau des indicateurs ##########
library(formattable)
library(WeightedCluster)

#' Fonction Creation_indicateur
#' 
#' @description  Cette fonction permet de récupérer les différents indicateurs de la fonction WeightedCluster::wcKMedoids et le nombre de groupes correspondant. Il permet de créer le paramètre indicateurs de la fonction tableau_cluster.
#' @param nb_cluster_min nombre minimal de groupes souhaité
#' @param nb_cluster_max nombre maximal de groupes souhaité
#' @param mat_dist matrice de distance
#' @param intialclust même paramètre que dans la fonction WeightedCluster::wcKMedoids
#'
#' @return un data.frame avec les différents indicateurs de la fonction WeightedCluster::wcKMedoids et le nombre de groupes correspondant
#' @export
Creation_indicateur<-function(nb_cluster_min,nb_cluster_max,mat_dist,intialclust){
  
  if (floor(nb_cluster_min)!=nb_cluster_min){
    stop(" 'nb_cluster_min' doit être un nombre entier ")
  }
  
  if (!(nb_cluster_min >0)){
    stop(" 'nb_cluster_min' doit être un positif ")
  }
  
  if (floor(nb_cluster_max)!=nb_cluster_max){
    stop(" 'nb_cluster_max' doit être un nombre entier ")
  }
  
  if (!(nb_cluster_max >0)){
    stop(" 'nb_cluster_max' doit être un positif ")
  }
  
  if(nb_cluster_max<nb_cluster_min){
    stop(" 'nb_cluster_max' doit être supérieur à 'nb_cluster_min' ")
  }
  
  if(!(is.matrix(mat_dist))){
    stop(" 'mat_dist' doit être une matrice ")
  }
  
tab_ind<-lapply((nb_cluster_min:nb_cluster_max), FUN=function(i){
  wcKMedoids(mat_dist, k=i, initialclust = intialclust)
})

v<-NULL
for (i in 1:(nb_cluster_max-nb_cluster_min+1)){
  v<-rbind(v,tab_ind[[i]]$stats)
}
v<-as.data.frame(cbind("Clusters"=c(nb_cluster_min:nb_cluster_max),v))
v<-v[,-c(8,10)]
liste_return<-list("dataFrame"=v,"tableau"=tab_ind)
return(liste_return)
}

#' @examples

#indicateur<-Creation_indicateur(4,6,multich.dist,seq.agnes)

###################################################################################################################

#' Fonction tableau_cluster
#'
#' @description  Cette fonction permet de mettre en forme le tableau de la fonction Creation_indicateur et de valoriser la meilleure valeur pour chaque indicateur de la fonction WeightedCluster::wcKMedoids. De plus, cette fonction donne le meilleur nombre de groupes à créer parmi les valeurs proposées. 
#' 
#' @param indicateurs un data.frame que l'on peut obtenir avec la fonction Creation_indicateur
#' @param nb_cluster_min nombre minimal de groupes souhaité
#' @param nb_cluster_max nombre maximal de groupes souhaité
#'
#' @return un tableau
#' @export

tableau_cluster<-function(indicateurs){
  
  ind=NULL
  for (i in (1:7)){
    ind<-cbind(ind,indicateurs[,c(2:8)][i]==apply(indicateurs[,c(2:8)],2,max)[i])
  }
  ind<-cbind(ind,indicateurs[,9]==min(indicateurs[,9]))
  indicateurs<-cbind(indicateurs,"Nb_indicateurs"=rowSums(ind))
  
  
  
  indi_max <- formatter("span", 
                        style = x ~ style(color = ifelse(x %in% max(x), "seagreen", "black"),
                                          "font-weight" = ifelse(x %in% max(x), "bold", NA)))
  indi_min <- formatter("span", 
                        style = x ~ style(color = ifelse(x %in% min(x), "seagreen", "black"),
                                          "font-weight" = ifelse(x %in% min(x), "bold", NA)))
  
  tab<-formattable(indicateurs, list(PBC = indi_max,
                                     HG = indi_max,
                                     HGSD = indi_max,
                                     ASW = indi_max,
                                     ASWw = indi_max,
                                     CH = indi_max,
                                     CHsq = indi_max,
                                     HC = indi_min,
                                     Clusters=formatter("span", 
                                                        style = x ~ style(color = ifelse(x %in% indicateurs[which(indicateurs$Nb_indicateurs==max(indicateurs$Nb_indicateurs)),"Clusters"], "springgreen", "black"),
                                                                          "font-weight" = ifelse(x %in% indicateurs[which(indicateurs$Nb_indicateurs==max(indicateurs$Nb_indicateurs)),"Clusters"], "bold", NA),
                                                                          "font-size"= ifelse(x %in% indicateurs[which(indicateurs$Nb_indicateurs==max(indicateurs$Nb_indicateurs)),"Clusters"], "x-large", "medium")))
  ))
  
  
  return(tab)
  
}

#' @examples

#tableau_cluster(indicateur$dataFrame)->tabForme


