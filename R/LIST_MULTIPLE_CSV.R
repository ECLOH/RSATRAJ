LIST_MULTIPLE_CSV<-function(emplacement.dossier="C:/Users/elie/Desktop/DOSSIER CSV", 
                            pattern.extension=".csv", 
                            autre.pattern="RSABEM", nom.list="ListDF", ...){
  message("Création des .csv ...")
  APPLY_READCSV_DOSSIER(, ...)->csv.list
  message("Enregistrement de la liste de .csv ...")
  save(csv.list, file = paste(emplacement.dossier, "/", nom.list, ".RData", sep=""))
  return(csv.list)
}
