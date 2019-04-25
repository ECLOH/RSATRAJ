#' Fonction Taille_graph_flux
#'
#' 
#' @param nb_grp : un entier
#'
#' @return une matrice donnant les positions et l'ordre des graphiques pour la fonction  marrangeGrob pour afficher les graphiques de flux pour chaque groupe
#' @export
#'
taille_graph_flux<-function(nb_grp){
  if (floor(nb_grp)!=nb_grp){
    stop(" 'nb_grp' doit être un nombre entier ")
  }
  
  if (!(nb_grp >0)){
    stop(" 'nb_grp' doit être un positif ")
  }
vect<-cbind(1,2)
if (nb_grp>2){
  for (i in 3:nb_grp){
    if((i%%2)==1){
      vect<-rbind(vect,c(i,i+1))
    }
  }
}
return(vect)
}


#' @examples
