################ Problème ne fonctionne plus s'il y a une période avec un seul état de représenté et que cette période est situé au milieu des graphiques de flux #########
######################################################## SOUS-GROUPE #################################################################################

#Fonction graphique de flux pour des sous-groupes avec "output de la fonction" $render()

#' Title
#'
#' @param data 
#' @param seq.data 
#' @param col_periode 
#' @param label 
#' @param cpal 
#' @param var_grp 
#' @param label_grp 
#' @param box_width 
#' @param box_label_cex 
#' @param arrow_type 
#' @param arrow_rez 
#' @param title 
#'
#' @return
#' @export
#'
flux_grp<- function(data,seq.data,col_periode,label=NULL,cpal=NULL,var_grp,label_grp,box_width=NULL,box_label_cex=NULL,arrow_type="simple",arrow_rez=NULL,title=NULL){
############################# Controle des paramètres de la fonction ###########################################
# Tester si l'utilisateur rentre le bon format pour les paramètres
if (!(is.vector(col_periode))){
  stop(" 'col_periode' doit être un vecteur ")
}
if (length(col_periode)<2){
  stop(" 'col_periode' doit avoir au moins 2 éléments ")
}
if (any(!(col_periode %in% colnames(seq.data)))){
  stop(" 'col_periode' doit contenir des varibles de la séquence ")
}
  

if (!(is.null(label)) && !(is.vector(label)) ){
  stop(" 'label' doit être un vecteur ")
}

if (!(is.character(var_grp))){
  stop(" 'var_grp' doit être de type caractère")
}

if (!(is.character(label_grp))){
  stop(" 'label_grp' doit être de type caractère")
}

#Tester si l'utilisateur a bien mis qu'une variable et un level pour identifier le groupe étudié  
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

###################################################################################################################################

#On regarde si l'utilisateur a précisé des labels, si ce n'est pas le cas les labels seront le nom des variables temporelles choisies
if (is.null(label)){
  label<-col_periode
}

transitions <- table(droplevels(data[,colnames(data)==col_periode[1]][data[,colnames(data)==var_grp]==label_grp]),droplevels(data[,colnames(data)==col_periode[2]][data[,colnames(data)==var_grp]==label_grp])) %>%
  getRefClass("Transition")$new(label=label[1:2])

#Création de la palette de couleur
if (is.null(cpal)){
  attr(seq.data, "cpal") <- brewer.pal(length(alphabet(seq.data)), "Set3")
} else {
  attr(seq.data, "cpal") <-cpal
}

output_perc <- 
  function(txt, n) sprintf("%s\n[%.0f%%]", txt, n)

#On crée séparément les deux premiers pas de temps
box_txt <-
  list(mapply(output_perc,
              txt = rownames(table(droplevels(data[,colnames(data)==col_periode[1]][data[,colnames(data)==var_grp]==label_grp]),droplevels(data[,colnames(data)==col_periode[2]][data[,colnames(data)==var_grp]==label_grp]))),
              n = prop.table(rowSums(table(droplevels(data[,colnames(data)==col_periode[1]][data[,colnames(data)==var_grp]==label_grp]),droplevels(data[,colnames(data)==col_periode[2]][data[,colnames(data)==var_grp]==label_grp]))))*100),
       mapply(output_perc,
              txt = colnames(table(droplevels(data[,colnames(data)==col_periode[1]][data[,colnames(data)==var_grp]==label_grp]),droplevels(data[,colnames(data)==col_periode[2]][data[,colnames(data)==var_grp]==label_grp]))),
              n = prop.table(colSums(table(droplevels(data[,colnames(data)==col_periode[1]][data[,colnames(data)==var_grp]==label_grp]),droplevels(data[,colnames(data)==col_periode[2]][data[,colnames(data)==var_grp]==label_grp]))))*100))

# Création d'un data frame qui permettra d'associer les bonnes couleurs pour chaque level
data_levels<-alphabet(seq.data)
data_couleur<-cpal(seq.data)
df_col_level<-cbind(data_levels,data_couleur)

t1<-names(box_txt[[1]])
transitions$fill_clr[[1]]<-df_col_level[which(df_col_level[,1]%in%t1),2]

t2<-names(box_txt[[2]])
transitions$fill_clr[[2]]<-df_col_level[which(df_col_level[,1]%in%t2),2]

#On choisit de mettre le texte en noir
transitions$txt_clr = list(c(rep("black",length(transitions$txt_clr[[1]]))),c(rep("black",length(transitions$txt_clr[[2]]))))

long<-length(col_periode)
#On s'occupe des autres périodes temporelles
if(long>2){
  for (i in 3:long){
    
    table(droplevels(data[,colnames(data)==col_periode[i-1]][data[,colnames(data)==var_grp]==label_grp]),droplevels(data[,colnames(data)==col_periode[i]][data[,colnames(data)==var_grp]==label_grp])) %>%
      transitions$addTransitions(label = label[i])
    
    box_txt[[i]]<-mapply(output_perc,
                         txt = colnames(table(droplevels(data[,colnames(data)==col_periode[i-1]][data[,colnames(data)==var_grp]==label_grp]),droplevels(data[,colnames(data)==col_periode[i]][data[,colnames(data)==var_grp]==label_grp]))),
                         n = prop.table(colSums(table(droplevels(data[,colnames(data)==col_periode[i-1]][data[,colnames(data)==var_grp]==label_grp]),droplevels(data[,colnames(data)==col_periode[i]][data[,colnames(data)==var_grp]==label_grp]))))*100)
    
    t<-names(box_txt[[i]])
    transitions$fill_clr[[i]]<-df_col_level[which(df_col_level[,1]%in%t),2]
    
    transitions$txt_clr[[i]] = rep("black",length(transitions$txt_clr[[i]]))
  }
}

#On affecte les états et pourcentages correspondants à l'objet transitions
transitions$box_txt = box_txt

# Paramètres graphiques
if (!(is.null(box_width))){
  transitions$box_width = box_width
}

if (!(is.null(box_label_cex))){
  transitions$box_label_cex = box_label_cex
}
transitions$arrow_type = arrow_type

if (!(is.null(arrow_rez))){
  transitions$arrow_rez = arrow_rez
}
transitions$title<-title

return(transitions)
}

#' @examples

tran<-flux_grp(data=contrat,seq.data = seq.contrat,col_periode=c("T2.2013","X2013.octobre","X2014.octobre","X2015.octobre","X2016.octobre"),label=c("T2-2013", "T4-2013", "T4-2014", "T4-2015", "T4-2016"),var_grp="acpam5",label_grp = "G1")
#Tracer le graphique
tran$render() 

#######################################################################################################################################################

#Fonction graphique de flux avec "output de la fonction" $render()

#' Title
#'
#' @param data 
#' @param seq.data 
#' @param col_periode 
#' @param label 
#' @param cpal 
#' @param box_width 
#' @param box_label_cex 
#' @param arrow_type 
#' @param arrow_rez 
#' @param title 
#'
#' @return
#' @export
  flux<- function(data,seq.data,col_periode,label=NULL,cpal=NULL,box_width=NULL,box_label_cex=NULL,arrow_type="simple",arrow_rez=NULL,title=NULL){
    ############################# Controle des paramètres de la fonction ###########################################
    # Tester si l'utilisateur rentre le bon format pour les paramètres
    if (!(is.vector(col_periode))){
      stop(" 'col_periode' doit être un vecteur ")
    }
    if (length(col_periode)<2){
      stop(" 'col_periode' doit avoir au moins 2 éléments ")
    }
    
    if (any(!(col_periode %in% colnames(seq.data)))){
      stop(" 'col_periode' doit contenir des varibles de la séquence ")
    }
    
    if (!(is.null(label)) && !(is.vector(label)) ){
      stop(" 'label' doit être un vecteur ")
    }

    ###################################################################################################################################
    
    #On regarde si l'utilisateur a précisé des labels, si ce n'est pas le cas les labels seront le nom des variables temporelles choisies
    if (is.null(label)){
      label<-col_periode
    }
    
    transitions <- table(droplevels(data[,colnames(data)==col_periode[1]]),droplevels(data[,colnames(data)==col_periode[2]])) %>%
      getRefClass("Transition")$new(label=label[1:2])
    
    #Création de la palette de couleur
    if (is.null(cpal)){
      attr(seq.data, "cpal") <- brewer.pal(length(alphabet(seq.data)), "Set3")
    } else {
      attr(seq.data, "cpal") <-cpal
    }
    
    output_perc <- 
      function(txt, n) sprintf("%s\n[%.0f%%]", txt, n)
    
    #On crée séparément les deux premiers pas de temps
    box_txt <-
      list(mapply(output_perc,
                  txt = rownames(table(droplevels(data[,colnames(data)==col_periode[1]]),droplevels(data[,colnames(data)==col_periode[2]]))),
                  n = prop.table(rowSums(table(droplevels(data[,colnames(data)==col_periode[1]]),droplevels(data[,colnames(data)==col_periode[2]]))))*100),
           mapply(output_perc,
                  txt = colnames(table(droplevels(data[,colnames(data)==col_periode[1]]),droplevels(data[,colnames(data)==col_periode[2]]))),
                  n = prop.table(colSums(table(droplevels(data[,colnames(data)==col_periode[1]]),droplevels(data[,colnames(data)==col_periode[2]]))))*100))
    
    # Création d'un data frame qui permettra d'associer les bonnes couleurs pour chaque level
    data_levels<-alphabet(seq.data)
    data_couleur<-cpal(seq.data)
    df_col_level<-cbind(data_levels,data_couleur)
    
    t1<-names(box_txt[[1]])
    transitions$fill_clr[[1]]<-df_col_level[which(df_col_level[,1]%in%t1),2]
    
    t2<-names(box_txt[[2]])
    transitions$fill_clr[[2]]<-df_col_level[which(df_col_level[,1]%in%t2),2]
    
    #On choisit de mettre le texte en noir
    transitions$txt_clr = list(c(rep("black",length(transitions$txt_clr[[1]]))),c(rep("black",length(transitions$txt_clr[[2]]))))
    
    long<-length(col_periode)
    #On s'occupe des autres périodes temporelles
    if(long>2){
      for (i in 3:long){
        
        table(droplevels(data[,colnames(data)==col_periode[i-1]]),droplevels(data[,colnames(data)==col_periode[i]])) %>%
          transitions$addTransitions(label = label[i])
        
        box_txt[[i]]<-mapply(output_perc,
                             txt = colnames(table(droplevels(data[,colnames(data)==col_periode[i-1]]),droplevels(data[,colnames(data)==col_periode[i]]))),
                             n = prop.table(colSums(table(droplevels(data[,colnames(data)==col_periode[i-1]]),droplevels(data[,colnames(data)==col_periode[i]]))))*100)
        
        t<-names(box_txt[[i]])
        transitions$fill_clr[[i]]<-df_col_level[which(df_col_level[,1]%in%t),2]
        
        transitions$txt_clr[[i]] = rep("black",length(transitions$txt_clr[[i]]))
      }
    }
    
    #On affecte les états et pourcentages correspondants à l'objet transitions
    transitions$box_txt = box_txt
    
    # Paramètres graphiques
    if (!(is.null(box_width))){
      transitions$box_width = box_width
    }
    
    if (!(is.null(box_label_cex))){
      transitions$box_label_cex = box_label_cex
    }
    transitions$arrow_type = arrow_type
    
    if (!(is.null(arrow_rez))){
      transitions$arrow_rez = arrow_rez
    }
    transitions$title<-title
    
    return(transitions)
  }

#' @examples

tran<-flux(data=contrat,seq.data = seq.contrat,col_periode=c("T2.2013","X2013.octobre","X2014.octobre","X2015.octobre","X2016.octobre"),label=c("T2-2013", "T4-2013", "T4-2014", "T4-2015", "T4-2016"))
# Tracer le graphique
tran$render()
