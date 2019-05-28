LIST_MULTIPLE_CSV<-function(emplacement.dossier="C:/Users/elie/Desktop/DOSSIER CSV", 
                            pattern.extension=".csv", 
                            autre.pattern="RSABEM", nom.list="ListDF", ...){
  Sys.time()->start
  message("Création des .csv ...")
  APPLY_READCSV_DOSSIER(, ...)->csv.list
  message("Enregistrement de la liste de .csv ...")
  save(csv.list, file = paste(emplacement.dossier, "/", nom.list, ".RData", sep=""))
  Sys.time()->STOP 
  message(paste("Temps écoulé :", difftime(time1 = STOP, time2=start, units = "mins"), "minutes"))
  return(csv.list)
}
