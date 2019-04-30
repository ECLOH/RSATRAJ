library(ggplot2)
library(ggalluvial)
library(tidyr)
library(dplyr)
contrat <- read.csv(#"G:/dgsd/Dopro/Projets/2018/E - Etude_RSA/E-RSA_Appli_Shiny_Trajectoires/Multichannel/T_final_accompagnement.csv",
                    head = TRUE, sep=";", colClasses=c(rep("factor",16)),  na.strings=c(""," " ,"NA"))
contrat2<-tidyr::gather(contrat[,1:5],"year","n",-NIR)
win.graph()
gg <- ggplot(contrat2,
             aes(x = year, stratum = n, alluvium = NIR,
                 fill = n,label=n)) +
  geom_lode() + geom_flow() +
  geom_stratum(alpha = 0) +
  geom_text(stat = "stratum")
gg

#Rajouter du texte dans les rectangles
#https://www.rdocumentation.org/packages/ggalluvial/versions/0.9.1/topics/geom_flow

contrat2$freq<-1
contrat3<-contrat2 %>% group_by(year) %>% mutate(prop=(freq/sum(freq))*100)
win.graph()
ggplot(contrat3,
       aes(x = year, stratum = n, alluvium = NIR,
           fill = n, label = prop)) +
  geom_lode() + geom_flow() +
  geom_stratum() +
  geom_text(stat = "stratum")


#stat_fill_labels() +
# source("https://install-github.me/larmarange/JLutils")
# library(JLutils)



# Test avec  une sous-population qui passe par une période où il n'y a qu'un seul état

data_cluster<-read.csv("C:/Temp/Git/Rstudio_appariement/RSATRAJ/data/T_accomp_apres_cluster.csv",head = TRUE, sep=";", colClasses=c(rep("factor",16)),  na.strings=c(""," " ,"NA"))
data.select<-data_cluster[which(data_cluster[,"CLI"]=="CHATEAUBRIANT" & data_cluster[,"acpam5"]=="G4"),]
data.select2<-tidyr::gather(data.select[,1:5],"year","n",-NIR)

win.graph()
gg <- ggplot(data.select2,
             aes(x = year, stratum = n, alluvium = NIR,
                 fill = n)) +
  geom_flow()+
  geom_stratum()+
  scale_y_continuous(breaks=NULL)+
  theme(panel.background = element_rect(fill = "white", colour="white"))
gg


# Test avec le graphique de flux du repère
data_cluster<-read.csv("C:/Temp/Git/Rstudio_appariement/RSATRAJ/data/T_accomp_apres_cluster.csv",head = TRUE, sep=";", colClasses=c(rep("factor",16)),  na.strings=c(""," " ,"NA"))
data.select<-data_cluster[which(data_cluster[,"acpam5"]=="G1"),]
data.select2<-tidyr::gather(data.select[,c(1,2,4,8,12,16)],"year","n",-NIR)

data.select2$freq<-1

data.select3<-data.select2 %>% group_by(year) %>% mutate(prop=(freq/sum(freq))*100)

win.graph()
ggplot(data.select3,
       aes(x = year, stratum = n, alluvium = NIR,
           fill = n,label=prop)) +
  geom_lode() + geom_flow() +
  geom_stratum() +
  geom_text(stat = "stratum")


################### Graphique de flux avec création des pourcentages à l'extérieur du graphique ##################################

                                ####### Selection des données ########
data_cluster<-read.csv("C:/Temp/Git/Rstudio_appariement/RSATRAJ/data/T_accomp_apres_cluster.csv",head = TRUE, sep=";", colClasses=c(rep("factor",16)),  na.strings=c(""," " ,"NA"))

data.select<-data_cluster[which(data_cluster[,"acpam5"]=="G1"),]

                      ######### On modifie la table pour pouvoir faire le graphique #########

data.select2<-tidyr::gather(data.select[,c(1,2,4,8,12,16)],"year","n",-NIR)

                    ################ On affecte à chaque individu le poid de 1 ################
data.select2$freq<-1

data.select2 %>% group_by(year,n) %>% summarise(nb=sum(freq))->dt
dt %>% group_by(year) %>% mutate(prop=sprintf("%.0f %%",(nb/sum(nb))*100))->dt2

                    ##### On enlève le label des pourcentages illisibles sur le graphique #####

dt2[dt2$prop== "0 %","prop"]<-""
dt2[dt2$prop== "1 %","prop"]<-""
dt2[dt2$prop== "2 %","prop"]<-""

                    ########## On enlève la colonne nb qui ne sert pas pour la suite ##########
dt2[,-c(3)]->dt2

          ##### On joint les données sélectionnées avec la table contenant les pourcentages des labels #####

left_join(data.select2,dt2,by=c("year","n"))->dt3

win.graph()
ggplot(dt3,
       aes(x = year, stratum = n, alluvium = NIR,
           fill = n,label=prop)) +
  geom_lode() + geom_flow() +
  geom_stratum(width=0.5) +
  geom_text(stat = "stratum",size=3)+
  labs(fill = "Légende")+
  scale_fill_brewer(palette="Set3")+ #change les couleurs utilisées pour le graphique
  ggtitle("Graphique de flux")+
  scale_y_continuous(breaks=NULL)+
  theme(panel.background = element_rect(fill = "white", colour="white"), #fond blanc du graphique
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) # centre le titre



