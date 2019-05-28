#' Fonction col_flux  
#' 
#' @description  Cette fonction permet d'avoir une légende universelle pour les graphiques de flux
#' @param data 
#' @param seq.data 
#' @param Pack.of.palettes "RColorBrewer" ou "wesanderson"
#' @param ... autres paramètres utilisés dans brewer.pal et wes_palette (name, n, type)
#'
#' @return un vecteur avec les différentes valeurs des couleurs et dont le nom des lignes est un level 
#' @export

col_flux<-function(data, seq.data, Pack.of.palettes="RColorBrewer"){
  colnames(seq.data)->col_nom
  which(colnames(data) %in% col_nom)->col_contrat
  vect_levels<-NULL
  for (i in col_contrat){
    rbind(vect_levels,cbind(levels(data[,i])))->vect_levels
  }
  nom<-unique(vect_levels)
  
  if(tolower(Pack.of.palettes)=="rcolorbrewer"){
  df<-brewer.pal(n = length(nom), ...)
  } else {
    if(tolower(Pack.of.palettes)=="wesanderson"){
      df<-wes_palette(n=length(nom), ...)
    } else {stop("Il faut choisir 'RColorBrewer' ou 'wesanderson'")}
  }
  names(df)<-nom
  
  return(df)
}

#' @examples
