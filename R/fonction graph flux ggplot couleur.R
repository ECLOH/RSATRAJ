################### Fonction graph de flux par groupe #####################
#' Fonction pour faire des graphiques de flux pour un groupe d'individus donné
#'
#' @param data 
#' @param seq_data 
#' @param col_periode 
#' @param var_grp 
#' @param label_grp 
#'
#' @return
#' @export
#'
graph_flux_grp<-function(data, seq_data, col_periode, var_grp,label_grp) {
  
  # Tester si l'utilisateur rentre le bon format pour les param?tres
  if (!(is.vector(col_periode))){
    stop(" 'col_periode' doit être un vecteur ")
  }
  if (length(col_periode)<2){
    stop(" 'col_periode' doit avoir au moins 2 éléments ")
  }
  if (any(!(col_periode %in% colnames(seq_data)))){
    stop(" 'col_periode' doit contenir des varibles de la séquence ")
  }
  if (!(is.character(var_grp))){
    stop(" 'var_grp' doit ?tre de type caractère")
  }
  
  if (!(is.character(label_grp))){
    stop(" 'label_grp' doit ?tre de type caractère")
  }
  
  #Tester si l'utilisateur a bien mis qu'une variable et un level pour identifier le groupe ?tudi?  
  if (length(var_grp)!=1){
    stop(" 'var_grp' ne doit contenir qu'un élément")
  }
  
  if (length(label_grp)!=1){
    stop(" 'label_grp' ne doit contenir qu'un élément ")
  }
  
  # Tester si l'utilisateur donne une variable et un level existants  
  if (!(var_grp %in% colnames(data))){
    stop(" 'var_grp' doit être une variable du jeu de données")
  }
  
  if (!(label_grp %in% levels(data[,colnames(data)==var_grp]))){
    stop(" 'label_grp' doit être un level de la variable 'var_grp' ")
  }
  
  ##### Couleur ######
  col_flux(data,seq_data)->df_col
  
  #### Selection des données ####
  data_select<-data[(data[,var_grp]==label_grp),col_periode]
  seq_select<-seq_data[(data[,var_grp]==label_grp),colnames(seq_data)%in%col_periode]
  
  #### Création d'un identifiant ######
  nb_ligne<-dim(data_select)[1]
  nb_col<-dim(data_select)[2]+1
  data_select[,nb_col]<-as.factor(1:nb_ligne)
  name_id<-colnames(data_select)[nb_col]
  
  data_select2<-tidyr::gather(data_select[,c(col_periode,name_id)],"Temps","Etats",-name_id)
  
  ################ On affecte à chaque individu le poid de 1 ################
  data_select2$freq<-1
  
  data_select2 %>% group_by(Temps,Etats) %>% summarise(nb=sum(freq))->dt1
  dt1 %>% group_by(Temps) %>% mutate(prop=sprintf("%.0f %%",(nb/sum(nb))*100))->dt2
  
  ##### On enl?ve le label des pourcentages illisibles sur le graphique #####
  
  dt2[dt2$prop== "0 %","prop"]<-""
  dt2[dt2$prop== "1 %","prop"]<-""
  dt2[dt2$prop== "2 %","prop"]<-""
  
  ########## On enlève la colonne nb qui ne sert pas pour la suite ##########
  dt2[,c("Temps","Etats","prop")]->dt2
  
  ##### On joint les données sélectionnées avec la table contenant les pourcentages des labels #####
  
  left_join(data_select2,dt2,by=c("Temps","Etats"))->dt3
  if (length(alphabet(seq_select))<=12){
    g<-ggplot(dt3,
              aes(x = Temps, stratum = Etats, alluvium = dt3[,name_id],
                  fill = Etats,label=prop)) +
      geom_lode() + geom_flow() +
      geom_stratum() +
      geom_text(stat = "stratum",size=3)+
      labs(fill = "Légende")+
      scale_fill_manual(values = df_col)+ #change les couleurs utilis?es pour le graphique
      ggtitle(paste("Graphique de flux du groupe",label_grp,"avec",nb_ligne,"individus."))+
      scale_y_continuous(breaks=NULL)+ # enlever l'axe des y
      theme(panel.background = element_rect(fill = "white", colour="white"), #fond blanc du graphique
            legend.position = "bottom",
            plot.title = element_text(hjust = 0.5)) # centre le titre
  } else{
    g<-ggplot(dt3,
              aes(x = Temps, stratum = Etats, alluvium = dt3[,name_id],
                  fill = Etats,label=prop)) +
      geom_lode() + geom_flow() +
      geom_stratum() +
      geom_text(stat = "stratum",size=3)+
      labs(fill = "Légende")+
      ggtitle(paste("Graphique de flux du groupe",label_grp,"avec",nb_ligne,"individus."))+
      scale_y_continuous(breaks=NULL)+ # enlever l'axe des y
      theme(panel.background = element_rect(fill = "white", colour="white"), #fond blanc du graphique
            legend.position = "bottom",
            plot.title = element_text(hjust = 0.5)) # centre le titre
  }
  return(g)
  
}
#' @examples


# g<-graph_flux_grp(data = data.select,seq_data = seq.data,col_periode=c("T2.2013","X2013.octobre"),var_grp="acpam5",label_grp="G3")
# win.graph()
# g


################### Fonction graph de flux global #####################
#' Fonction pour faire des graphiques de flux sans les groupes
#'
#' @param data 
#' @param seq_data 
#' @param col_periode 
#'
#' @return un graphique
#' @export
#'
graph_flux<-function(data, seq_data, col_periode) {
  
  # Tester si l'utilisateur rentre le bon format pour les param?tres
  if (!(is.vector(col_periode))){
    stop(" 'col_periode' doit être un vecteur ")
  }
  if (length(col_periode)<2){
    stop(" 'col_periode' doit avoir au moins 2 éléments ")
  }
  if (any(!(col_periode %in% colnames(seq_data)))){
    stop(" 'col_periode' doit contenir des varibles de la séquence ")
  }
  
  ##### Couleur #####
  col_flux(data,seq_data)->df_col
  
  #### Selection des données ####
  data_select<-data[,col_periode]
  seq_select<-seq_data[,colnames(seq_data)%in%col_periode]
  
  #### Création d'un identifiant ######
  nb_ligne<-dim(data_select)[1]
  nb_col<-dim(data_select)[2]+1
  data_select[,nb_col]<-as.factor(1:nb_ligne)
  name_id<-colnames(data_select)[nb_col]
  
  data_select2<-tidyr::gather(data_select[,c(col_periode,name_id)],"Temps","Etats",-name_id)
  
  ################ On affecte à chaque individu le poid de 1 ################
  data_select2$freq<-1
  
  data_select2 %>% group_by(Temps,Etats) %>% summarise(nb=sum(freq))->dt1
  dt1 %>% group_by(Temps) %>% mutate(prop=sprintf("%.0f %%",(nb/sum(nb))*100))->dt2
  
  ##### On enlève le label des pourcentages illisibles sur le graphique #####
  
  dt2[dt2$prop== "0 %","prop"]<-""
  dt2[dt2$prop== "1 %","prop"]<-""
  dt2[dt2$prop== "2 %","prop"]<-""
  
  ########## On enlève la colonne nb qui ne sert pas pour la suite ##########
  dt2[,c("Temps","Etats","prop")]->dt2
  
  ##### On joint les données sélectionnées avec la table contenant les pourcentages des labels #####
  
  left_join(data_select2,dt2,by=c("Temps","Etats"))->dt3
  if (length(alphabet(seq_select))<=12){
    g<-ggplot(dt3,
              aes(x = Temps, stratum = Etats, alluvium = dt3[,name_id],
                  fill = Etats,label=prop)) +
      geom_lode() + geom_flow() +
      geom_stratum() +
      geom_text(stat = "stratum",size=3)+
      labs(fill = "Légende")+
      scale_fill_manual(values = df_col)+ #change les couleurs utilis?es pour le graphique
      ggtitle(paste("Graphique de flux avec",nb_ligne,"individus."))+
      scale_y_continuous(breaks=NULL)+ # enlever l'axe des y
      theme(panel.background = element_rect(fill = "white", colour="white"), #fond blanc du graphique
            legend.position = "bottom",
            plot.title = element_text(hjust = 0.5)) # centre le titre
  } else{
    g<-ggplot(dt3,
              aes(x = Temps, stratum = Etats, alluvium = dt3[,name_id],
                  fill = Etats,label=prop)) +
      geom_lode() + geom_flow() +
      geom_stratum() +
      geom_text(stat = "stratum",size=3)+
      labs(fill = "Légende")+
      ggtitle(paste("Graphique de flux avec",nb_ligne,"individus."))+
      scale_y_continuous(breaks=NULL)+ # enlever l'axe des y
      theme(panel.background = element_rect(fill = "white", colour="white"), #fond blanc du graphique
            legend.position = "bottom",
            plot.title = element_text(hjust = 0.5)) # centre le titre
  }
  return(g)
  
}

#' @examples

# g<-graph_flux(data = contrat,seq_data = seq.contrat,col_periode=c("T2.2013","X2013.octobre"))
#  win.graph()
#  g