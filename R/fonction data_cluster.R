###### Fonction qui rajoute la colonne des groupes #######
#' Title
#'
#' @param tabl_ind liste retournée par la fonction "Creation_indicateur"
#' @param data jeu de données
#' @param nb_groupe Nombre de groupes souhaité
#'
#' @return un data frame avec en dernière colonne les groupes
#' @export
#' 
data_cluster<-function(tabl_ind,data,nb_groupe){
  if (floor(nb_groupe)!=nb_groupe){
    stop(" 'nb_groupe' doit être un nombre entier ")
  }
  
  if (!(nb_groupe >0)){
    stop(" 'nb_groupe' doit être un positif ")
  }
  
  dataCopy<-data
  long_data<-length(dataCopy)+1
  
  n<-which(tabl_ind$dataFrame[1]==nb_groupe)
  
  dataCopy[,long_data]<-as.factor(tabl_ind$tableau[[n]]$clustering)
  colnames(dataCopy)[long_data]<-"Clustering"
  
  
  vect_levels<-sapply(1:nb_groupe, FUN=function(i){
    paste0("G",i)
  })
  
  levels(dataCopy$Clustering) <- vect_levels
  return(dataCopy)
}

#' @examples
#' 
#data_cluster(tabl_ind=indicateur,data=contrat,nb_groupe=4)->dTest
