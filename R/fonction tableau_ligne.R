# http://olivier.godechot.free.fr/hoparticle.php?id_art=465
# https://stackoverflow.com/questions/39757897/using-formattable-in-r-with-dynamic-column-headers

#' Title
#'
#' @param data data frame
#' @param var_grp variable contenant la classification
#' @param var variable étudiée
#'
#' @return un data frame mis en forme
#' @export

tableau_ligne<-function(data,var_grp,var){
  
  if(!(is.data.frame(data))){
    stop(" 'data' doit être un data frame ")
  }
  
  if(!(var_grp %in% colnames(data))){
    stop(" 'var_grp' doit être une variable du jeu de donnees ")
  }
  
  if(!(var %in% colnames(data))){
    stop(" 'var' doit être une variable du jeu de donnees ")
  }
  
  round(prop.table(addmargins(table(data[,var_grp],data[,var]),1),1),2)->tableau
  dim(tableau)[1]->NumGlobal
  rownames(tableau)[NumGlobal]<-"Global"
  
  as.data.frame(as.matrix.data.frame(tableau))->tabDf
  colnames(tabDf)<-colnames(tableau)
  rownames(tabDf)<-rownames(tableau)
  
  
  # On colorie les valeurs supérieur à la valeur dans l'ensmeble du jeu de données
  # On multiplie par le nombre de modalités etudiées
  SupGlobalTableau <- rep(list(formatter("span", 
                                         style = x ~ style(color = ifelse(x > x[NumGlobal], "seagreen", "black"),"font-weight" = ifelse(x > x[NumGlobal], "bold", NA)))),dim(tableau)[2])
  
  #On affecte le nom des modalités étudiées pour automatiser la fonction formattable
  names(SupGlobalTableau)<-colnames(tabDf)
  
  tab<-formattable(tabDf, SupGlobalTableau)%>%as.datatable()  #permet de convertir et d'utiliser un renderDataTable dans shiny
  return(tab)
}

#' @examples


