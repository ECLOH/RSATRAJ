APPLY_XML_CSV_DOSSIER<-function(emplacement.dossier = "C:/Users/019294/Desktop/XML_IMPORT/DOSSIER_IMPORT_XML"){
  list.files(emplacement.dossier)->fichiers
  lapply(fichiers, function(file.name){
    paste(emplacement.dossier, file.name, sep="/")
  })->list.files
  list.files[grepl(pattern=".xml", x = list.files, fixed=TRUE)]->list.xml.files
  message(paste("Nombre de fichers xml dans le dossier: ", length(list.xml.files)))
  lapply(list.xml.files, function(listi){
    list("SOURCE"=listi, "DESTINATION"=gsub(pattern = ".xml", replacement = ".csv", x = listi, fixed = TRUE))
  })->list.de.noms
  lapply(list.de.noms, FUN = function(listi){
    message(paste("FROM :",  listi$SOURCE, "| TO :", listi$DESTINATION))
    message("...")
    
    XML_TO_CSV(chemin.xml = listi$SOURCE, chemin.destination = listi$DESTINATION, return.df=FALSE, save.csv=TRUE)
  })
}
