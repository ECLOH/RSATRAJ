#' @author Elie
#' @description  ...
#' @title La fonction ""seqdef_modgap""XML_DATA"
#' @description  l'objectif de cette fonction est de charger les données présentes dans de multiples objets xml
#' @param minimal.gap numeric. nombre minimum d'états manquant consécutifs pouvant être considérés comme un "vrai" gap (une sortie du RSA)
#' @param regle.pour.faux.gap Choix entre: c("before", "after"). Si "before" ("after"), les faux gap sont recodés avec l'état précédent (suivant) non manquant.  
#' @return une liste de data.frame
#' @export
XML_DATA<-function(emplacement.dossier, ID.var){
  ####
  list.files(emplacement.dossier)->fichiers
  lapply(fichiers, function(file.name){
    paste(emplacement.dossier, file.name, sep="")
  })
  ####
  library(xml2)
  library(tidyverse)
  xml2::read_xml(x = "data/trajs_short.xml")->xml.data
  xml2::as_list(xml.data)->list.xml.data
  ####
  unlist(list.xml.data$Workbook$Worksheet$Table$Row)->vector.names
  length(vector.names)->nb.cols
  matrix(unlist(list.xml.data$Workbook$Worksheet), ncol = nb.cols, byrow = TRUE)->la.mat
  data.frame(la.mat[-c(1) , ])->la.df
  names(la.df)<-la.mat[1 , ]
  ####
  return(la.df)
}
