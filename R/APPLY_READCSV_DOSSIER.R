APPLY_READCSV_DOSSIER<-function(emplacement.dossier="C:/Users/elie/Desktop/DOSSIER CSV", 
                                pattern.extension=".csv", 
                                autre.pattern="RSABEM", all=TRUE){
  list.files(emplacement.dossier)->fichiers
  lapply(fichiers, function(file.name){
    paste(emplacement.dossier, file.name, sep="/")
  })->list.files
  list.files[grepl(pattern=pattern.extension, x = list.files, fixed=TRUE)&grepl(pattern=autre.pattern, x = list.files, fixed=TRUE)]->list.csv.files
  message(paste("Nombre de fichers csv dans le dossier: ", length(list.csv.files)))
  unlist(list.csv.files)->vec.csv.files
  donnees.mois<-lapply(vec.csv.files, FUN = function(listi){
     message(paste("FROM :",  listi))
     message("...")
     read.csv(file = listi, header = TRUE, encoding = "UTF-8", stringsAsFactors = FALSE)
  })
  gsub(pattern = paste(emplacement.dossier, "/", sep=""), replacement = "", x = vec.csv.files)->vecnames
  names(donnees.mois)<-vecnames
  return(donnees.mois)
}
