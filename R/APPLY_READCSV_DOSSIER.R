APPLY_READCSV_DOSSIER<-function(emplacement.dossier="C:/Users/elie/Desktop/DOSSIER CSV", 
                                pattern.extension=".csv", 
                                autre.pattern="RSABEM"){
  list.files(emplacement.dossier)->fichiers
  lapply(fichiers, function(file.name){
    paste(emplacement.dossier, file.name, sep="/")
  })->list.files
  if(!is.null(autre.pattern)){
  list.files[grepl(pattern=pattern.extension, 
                   x = list.files, fixed=TRUE)&grepl(pattern=autre.pattern, 
                                                     x = list.files, fixed=TRUE)]->list.csv.files
  } else {
    list.files[grepl(pattern=pattern.extension, 
                     x = list.files, fixed=TRUE)]->list.csv.files
  }
  message(paste("Nombre de fichers csv dans le dossier: ", length(list.csv.files)))
  unlist(list.csv.files)->vec.csv.files
  ####
  df.RSA.value<-data.frame("TEXT"=c("0"="Nouvelle demande en attente de décision CG pour ouverture du droit",
                      "1"="Droit refusé", 
                      "2"="Droit ouvert et versable",
                      "3"="Droit ouvert et suspendu (le montant du droit est calculable, mais l'existence du droit est remis en cause)",
                      "4"="Droit ouvert mais versement suspendu (le montant du droit n'est pas calculable)",
                      "5"="Droit clos",
                      "6"="Droit clos sur mois antérieur ayant eu un contrôle dans le mois de référence pour une période antérieure"), "MODALITE"=as.character(0:6))
  ###
  donnees.mois<-lapply(1:length(vec.csv.files), FUN = function(i){
    vec.csv.files[i]->listi
    message(paste(i, "/", length(vec.csv.files), "---------------",  sep=" "))
     message(paste("FROM :",  listi))
     message("...")
     read.csv(file = listi, header = TRUE, encoding = "UTF-8", stringsAsFactors = FALSE)->data.i
     #### AJOUT DE DATE ####
     message("DATE...")
     unlist(strsplit(x = unique(data.i$FICHIER_DESTINATION), split = c("."),  fixed = TRUE))[3]->DATE
     paste("01", substr(DATE, 3, 4), substr(DATE, 5, 8), sep="/")->a 
     b<-as.Date(a,format="%d/%m/%Y")
     data.i$DATE<-b
     #### AJOUT DE RSA ###
     message("RSA_simple...")
     sapply(1:nrow(data.i), FUN = function(i){
       as.character(data.i$PrestationRSA.SituationDossierRSA.EtatDossierRSA.ETATDOSRSA)->varRSA
       if(is.na(varRSA[i])){
         NA
       } else {
       if(varRSA[i]%in%c("5", "6", "1")){
         "NO.RSA"
       } else {
         if(varRSA[i]%in%c("3", "4")){
           "SUSPENDU"
         } else {
           if(varRSA[i]=="2"){
             "RSA"
           }
         }
       }
       }
     })->data.i$RSA_simple
     message("ETATDOSRSA_TEXT...")
     sapply(1:nrow(data.i), FUN = function(i){
       as.character(data.i$PrestationRSA.SituationDossierRSA.EtatDossierRSA.ETATDOSRSA)->varRSA
     as.character(subset(df.RSA.value, df.RSA.value$MODALITE==varRSA[i])$TEXT)
     })->data.i$ETATDOSRSA_TEXT
     data.i
  })
  gsub(pattern = paste(emplacement.dossier, "/", sep=""), replacement = "", x = vec.csv.files)->vecnames
  unlist(lapply(donnees.mois, function(x){unique(as.character(x$DATE))}))->vecnames
  names(donnees.mois)<-vecnames
  return(donnees.mois)
}
