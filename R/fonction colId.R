
#' Fonction colId
#'
#' @param df une table data.frame
#'
#' @return les indices de colonnes pouvant être des identifiants
#' @export

colId <- function(df){
  idcol=NULL
  for (i in 1:dim(df)[2]){
    if (length(unique(df[,i]))==length(df[,i])){
      idcol[i]<-i
    }
  }
  idcol<-idcol[!is.na(idcol)]
  return(idcol)
}

#' @examples
