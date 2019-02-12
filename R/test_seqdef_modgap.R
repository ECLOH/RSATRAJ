RSA.inter<-c(1, 2)
nb.mois<-5
mat.inter<-matrix(unlist(lapply(1:5, function(i){
  sample(x = RSA.inter, size = 1, replace = FALSE)->debuti
  c(rep("RSA perçu", times=debuti), rep(NA, times=nb.mois-(2*debuti)), rep("RSA perçu", times=debuti))
})), nrow=5, byrow=TRUE)
mat.deb<-matrix(nrow = 6, byrow = TRUE,  data =  c(c(NA, NA, "RSA perçu", "RSA perçu", "RSA perçu"),
c(NA, NA, NA, "RSA perçu", "RSA perçu"),
c("RSA perçu", "RSA perçu", "RSA perçu", NA, NA),
c(NA, "RSA perçu", "RSA perçu", "RSA perçu", NA),
c("RSA perçu", "RSA perçu", "RSA perçu", "RSA perçu", "RSA perçu"),
c("RSA perçu", NA, NA, "RSA perçu", NA)))

data=mat.inter; gaps = "VRAI_GAP"; regle.pour.faux.gap = "before"; minimal.gap = 3
mat.inter.seq<-seqdef_modgap(rbind(mat.inter, mat.deb), right = "RIGHT", gaps = "SORTIE", regle.pour.faux.gap = "before", minimal.gap = 2);mat.inter.seq
