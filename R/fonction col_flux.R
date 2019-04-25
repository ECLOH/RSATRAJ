#' Fonction col_flux  
#' 
#' @description  Cette fonction permet d'avoir une légende universelle pour les graphiques de flux
#' @param data 
#' @param seq.data 
#'
#' @return un vecteur avec les différentes valeurs des couleurs et dont le nom des lignes est un level 
#' @export

col_flux<-function(data,seq.data){
  colnames(seq.data)->col_nom
  which(colnames(data) %in% col_nom)->col_contrat
  vect_levels<-NULL
  for (i in col_contrat){
    rbind(vect_levels,cbind(levels(data[,i])))->vect_levels
  }
  nom<-unique(vect_levels)
  
  df<-brewer.pal(length(nom), "Paired")
  names(df)<-nom
  
  return(df)
}

#' @examples
