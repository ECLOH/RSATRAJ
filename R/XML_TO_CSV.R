#' @author Elie
#' @description  ...
#' @title La fonction "XML_TO_CSV"
#' @description  l'objectif de cette fonction est de charger les données présentes dans de multiples objets xml
#' @return un df et un csv enregistré
#' @export
XML_TO_CSV<-function(chemin.xml="rsa.RSABEM.00122018.A0110442.xml/rsa.RSABEM.00122018.A0110442.xml", 
                     chemin.destination="RSABEM_201812.csv", return.df=FALSE, save.csv=TRUE ){
  options(stringsAsFactors=FALSE)
  library(xml2)
  library(tidyverse)
  library(plyr)
  message(paste("Fichier à importer : ", chemin.xml))
  message("Debut de l'import du xml...")
  read_xml(x = chemin.xml)->xml.data#"rsa.RSABEM.00122018.A0110442.xml/rsa.RSABEM.00122018.A0110442.xml")->xml.data
  message("...fait")
  message("Debut de la conversion en liste....(30-45 mn env)")
  Sys.time()->heure.debut
  message(heure.debut)
  as_list(xml.data)->xml.list ## 30mn
  message("...fait")
  ####
  names(unlist(xml.list$Racine$InfosFoyerRSA))->vector.names
  length(vector.names)->nb.cols
  ####
  unique(unlist(lapply(xml.list$Racine, FUN = function(j){
    names(unlist(j))
  })))->glo.names
  data.frame(
    matrix(rep(NA, times=length(glo.names)), ncol = length(glo.names))
  )->data1
  names(data1)<-glo.names
  ####
  message("Création des data.frames individuelles....(5 mn env)")
  lapply(X = 1:length(xml.list$Racine), FUN = function(i){
    xml.list$Racine[[i]]->list.for.i
    names(unlist(list.for.i))->vector.names.i
    length(vector.names.i)->nb.cols.i
    matrix(unlist(list.for.i), ncol = nb.cols.i, byrow = TRUE)->la.mat
    data.frame(la.mat, stringsAsFactors = FALSE)->la.df
    names(la.df)<-vector.names.i
    glo.names[glo.names%in%vector.names.i]->glo.names.i
    la.df[ , glo.names.i]->la.df.last
    row.names(la.df.last)<-i
    la.df.last
  })->list.of.df # 4 mn
  message("....fait")
  
  #save(list.of.df, file="LISTOFDF.RData")
  lapply(list.of.df, FUN = dim)->list.dims
  list.of.df2<-list.of.df[-c(1)]
  
  lapply(1:length(list.of.df2), FUN = function(listi){
    list.of.df2[listi][[1]]
  })->list.of.df3
  message("Combinaison des data.frames individuelles...(5 mn)")
  do.call("rbind.fill", list.of.df3)->final.df
  final.df$FICHIER_SOURCE<-chemin.xml
  final.df$FICHIER_DESTINATION<-chemin.destination
  
  message("....fait")
  if(save.csv==TRUE){
    message("Ecriture du csv")
    write.csv(x = final.df, file = chemin.destination)
    message("....fait")
    message(paste("Fichier créé: ", chemin.destination))
  }
  if(return.df==TRUE){
    return(final.df)
  }
}


# XML_TO_CSV_OLD<-function(chemin.xml="rsa.RSABEM.00122018.A0110442.xml/rsa.RSABEM.00122018.A0110442.xml", 
#                      chemin.destination="RSABEM_201812.csv", return.df=FALSE, save.csv=TRUE ){
#   library(xml2)
#   library(tidyverse)
#   library(plyr)
#   message(paste("Fichier à importer : ", chemin.xml))
#   message("Debut de l'import du xml...")
#   read_xml(x = chemin.xml)->xml.data#"rsa.RSABEM.00122018.A0110442.xml/rsa.RSABEM.00122018.A0110442.xml")->xml.data
#   message("...fait")
#   message("Debut de la conversion en liste....(30-45 mn env)")
#   Sys.time()->heure.debut
#   message(heure.debut)
#   as_list(xml.data)->xml.list ## 30mn
#   message("...fait")
#   
#   #save(xml.list, file = "XMLLIST1.RData")
#   #load(file = "XMLLIST1.RData")
#   #length(xml.list[[1]])
#   ####
#   names(unlist(xml.list$Racine$InfosFoyerRSA))->vector.names
#   length(vector.names)->nb.cols
#   ####
#   unique(unlist(lapply(xml.list$Racine, FUN = function(j){
#     names(unlist(j))
#   })))->glo.names
#   data.frame(
#     matrix(rep(NA, times=length(glo.names)), ncol = length(glo.names))
#   )->data1
#   names(data1)<-glo.names
#   ####
#   message("Création des data.frames individuelles....(5 mn env)")
#   lapply(X = 1:length(xml.list$Racine), FUN = function(i){
#     xml.list$Racine[[i]]->list.for.i
#     names(unlist(list.for.i))->vector.names.i
#     length(vector.names.i)->nb.cols.i
#     matrix(unlist(list.for.i), ncol = nb.cols.i, byrow = TRUE)->la.mat
#     data.frame(la.mat)->la.df
#     names(la.df)<-vector.names.i
#     glo.names[glo.names%in%vector.names.i]->glo.names.i
#     la.df[ , glo.names.i]->la.df.last
#     row.names(la.df.last)<-i
#     la.df.last
#   })->list.of.df # 4 mn
#   message("....fait")
#   
#   #save(list.of.df, file="LISTOFDF.RData")
#   lapply(list.of.df, FUN = dim)->list.dims
#   list.of.df2<-list.of.df[-c(1)]
#   
#   lapply(1:length(list.of.df2), FUN = function(listi){
#     list.of.df2[listi][[1]]
#   })->list.of.df3
#   message("Combinaison des data.frames individuelles...(5 mn)")
#   do.call("rbind.fill", list.of.df3)->final.df
#   message("....fait")
#   if(save.csv==TRUE){
#   message("Ecriture du csv")
#   write.csv(x = final.df, file = chemin.destination)
#   message("....fait")
#   message(paste("Fichier créé: ", chemin.destination))
#   }
#   if(return.df==TRUE){
#   return(final.df)
#   }
# }
