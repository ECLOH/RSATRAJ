#### CES FONCTIONS PERMETTENT DE CALCULER LE TEMPS DE CALCUL DE SEQDIST POUR UNE AMPLITUDE CHOISIE DE NOMBRE DE TRAJECTOIRES, ET DE TROUVER LE TEMPS SIMULE POUR TOUT NOMBRE DE TRAJECTOIRE. 
times.dist<-function(amplit=seq(from = 0, to=5000, by=200)){#}, nb.sequences=10000){
  if(amplit[1]==0){amplit[1]<-1}
lapply(X =amplit , FUN = function(i){
  if(i==0){i<-1}
  Sys.time()->t1
  seqdist(trajs[1:i , ], method = "OM", sm = "TRATE")->temp
  Sys.time()->t2
  as.numeric(difftime(time1 = t2, time2 = t1, units = "secs")*1000)
})->times.dist
names(times.dist)<-amplit
data.frame(do.call("rbind", times.dist))->df
df$amplit<-as.numeric(as.character(row.names(df)))
names(df)<-c("time", "amplit")
return(df)
}
#
#times.dist()->calculated.times.for.dist
#
predict.time.dist<-function(df=calculated.times.for.dist, nb.sequences=10000){
fit_nls <- nls(time ~ a*(amplit ^ b), data = df[ , ], start =  c(a=0.5, b = 1), trace = F, control=nls.control(maxiter=1000))
pp<-ggplot(data = df)+geom_point(aes(x=amplit, y=time))+
  geom_line(aes(y=predict(fit_nls, newdata = data.frame(amplit = amplit)), x=amplit))
#
fit_nls$m$predict(newdata = data.frame(amplit=1:nb.sequences))->c.predict
nb.secondes<-round(c.predict[nb.sequences]/1000, 2)
nb.minutes<-round(nb.secondes/60, 2)
message(paste("Nombre de secondes pour", nb.sequences, "trajectoires :", nb.secondes, sep=" "))
message(paste("\n nombre de minutes :",  nb.minutes, sep=" "))
message(paste(sep=" ", "Estimation réalisée sur", nrow(calculated.times.for.dist), "lancements de seqdist()", "pour un nombre de trajectoires allant de", min(df$amplit), "à", max(df$amplit)))
return(list("nb.secondes"=nb.secondes, "nb.minutes"=nb.minutes, "pp"=pp))
}
#predict.time.dist(nb.sequences = 1100)