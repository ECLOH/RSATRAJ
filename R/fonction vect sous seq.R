#### Fonction pour sous-séquneces choisies ####
#' Title
#'
#' @param data un data frame dans lequel une colonne correspond à un état et une ligne à une succession d'état. Le nombre d'états successifs est compris
#' entre 2 et 3.
#'
#' @return un vecteur mise en forme pour être utilisé dans la fonction subsequence comme valeur pour l'argument str.subseq
#' @export
vect.sous.seq<-function(data){
  if (!(is.data.frame(data))){
    stop(" 'data' doit être un data frame ")
  }
  vect.sous.seq<-NULL
  for (i in (1:nrow(data))){
    if (data[i,3]==" "){
      text<-paste("(",data[i,1],")-(",data[i,2],")",sep="")
      vect.sous.seq<-rbind(vect.sous.seq,text)
    } else {
      text2<-paste("(",data[i,1],")-(",data[i,2],")-(",data[i,3],")",sep="")
      vect.sous.seq<-rbind(vect.sous.seq,text2)
    }
  }
  return(vect.sous.seq)
}


#' @examples
 
