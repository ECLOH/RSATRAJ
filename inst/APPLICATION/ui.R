#### packages ####
library(shiny)
library(TraMineR)
library(ggplot2)
library(TraMineRextras)
library(RColorBrewer)
library(DT)
library(RColorBrewer)
library(wesanderson)
library(shinythemes)
library(shinycssloaders)
library(cluster)
library(fastcluster)
library(gridExtra)
library(ggalluvial)
library(tidyr)
library(dplyr)
library(shinyWidgets)
library(formattable)
library(WeightedCluster)
library(stringr)
options(shiny.maxRequestSize=700*1024^2) 
#### UI ####

ui <- shinyUI(navbarPage('RSATRAJ', id="page", collapsible=TRUE, inverse=FALSE,theme=shinytheme("flatly"),#fluidPage(theme = shinytheme("flatly"),
                         tabPanel("Paramètres de la session",
                                  #tabsetPanel(id = "tabpan",
                                  #            tabPanel(title = "Paramètres de la session: ",
                                  sidebarPanel(
                                    h3("Chargement des fichiers de données"), 
                                    width = 6,
                                    shiny::selectInput(inputId = "DataType", label = "Choix du type de données", 
                                                       choices = c("Un objet RData contenant de multiples data.frame"="objet", 
                                                                   "Un seul fichier.csv contenant des données prêtes à l'emploi"="fichier"), 
                                                       multiple = FALSE, selected = "fichier"),
                                    conditionalPanel(
                                      condition = "input.DataType == 'fichier'",
                                
                                      fileInput(inputId="file1", label="Sélectionnez votre fichier source:", 
                                              multiple = FALSE, accept = NULL, width = NULL),
                                      shiny::selectInput(inputId="sepcol", label= "Separateur de colonnes", choices=c("Virgule" = ",","Point-Virgule" = ";","Tabulation" = "\t"), selected=","),
                                      shiny::selectInput(inputId="dec", label= "Décimal", choices=c("Virgule" = ",","Point" = "."), selected="."),
                                      shiny::selectInput(inputId="endoding", label= "Encoding ?", choices=c(UTF8 = "UTF-8", Latin1 = "latin1"), selected = "UTF-8", multiple = FALSE, width = "50%"),
                                      shiny::checkboxInput(inputId = "header", label="Header ?",value=FALSE),  
                                      shiny::checkboxInput(inputId = "rowname", label="Rownames ?",value=FALSE),
                                      conditionalPanel(
                                        condition = "input.rowname == true",
                                        shiny::selectInput(inputId="rownames_par", label="Variable rowname", 
                                                           choices = "", multiple = FALSE,selected = NULL, selectize = TRUE)),
                                      shiny::selectInput(inputId = "na", label = "na.strings", choices = c("Vide" , "Espace" = " ", "NA" = "NA"), selected = "NA", multiple = TRUE, selectize = TRUE)),
                                    conditionalPanel(
                                      condition = "input.DataType == 'objet'",
                                      h5("INFO: pour des raisons de sécurité il n'est pas possible de charger directement un dossier dans un navigateur web."),
                                      h5("Vous pouvez utiliser la fonction LIST_MULTIPLE_CSV du package RSATRAJ pour créer l'objet RData à partir de mulitples fichiers .csv"),
                                      fileInput(inputId="objetFile", 
                                                label="Sélectionner l'objet .RData contenant les multiples data.frame", 
                                                multiple = TRUE, accept = NULL, width = NULL),
                                      shiny::textOutput("CONTROLDATA"))
                                  ),
                                  sidebarPanel(
                                    h3("Paramétrage des trajectoires"),
                                    width = 6,
                                  shiny::selectInput(inputId = "timecol", label = "Variables temporelles", choices = "", selected = "PrestationRSA.SituationDossierRSA.EtatDossierRSA.ETATDOSRSA", multiple = TRUE, selectize = TRUE),
                                  shiny::uiOutput("DATA_UI"),
                                  shiny::selectInput(inputId = "mode", label = "Mode de travail:", 
                                                     choices = c("Flux d'entrants", "Flux en continu"), 
                                                     selected = c("Flux en continu"), multiple = FALSE),
                                  shiny::numericInput(inputId = "criterNb", label = "Critère de sortie : nombre de mois consécutifs",value = 3, min = 1, max = 36, step = 1),
                                  shiny::actionButton(inputId = "ValidParametres", label = "Je valide ces paramètres")
                                  
                                  
                                  #,textOutput("DATE_RANGE_1")
                         )
                         ,
                         mainPanel(
                           shiny::dataTableOutput("contenu")
                           )),


                         tabPanel("Statistiques descriptives",
                                  tabsetPanel(
                                    tabPanel(title = "Taux de transition ( et taux de sortie) ",
                                             shiny::numericInput(inputId = "PAStrate", label = "Pas de temps pour le calcul des taux de transition",value = 1, min = 1, max = 36, step = 1),
                                             shiny::checkboxInput(inputId = "TYPEtrate", label = "Les taux de transitions varient-ils avec le temps?",value = FALSE),
                                             textOutput("infotrate"),
                                             uiOutput("dt")
                                             #dataTableOutput('TRAJS_TRATE')
                                    ),
                                    tabPanel(title = "Représentation des trajectoires ",
                                             fluidRow(
                                               column(2,
                                                shiny::selectInput(inputId = "plottype", label = "Quel graphique voulez-vous représenter? ", choices = c("d", "f", "I", "ms", "mt", "r","Graphique de flux"="flux","Sous-séquences triées selon leur support"="sous.seq","Sous-séquences choisies"="sous.seq.ch"), selected = "d", multiple = FALSE),
                                                conditionalPanel(condition="input.plottype=='sous.seq.ch'",
                                                                 wellPanel(shiny::selectInput(inputId = "par.sous.seq1",label = "Etat 1",choices = "",multiple = FALSE),
                                                                     shiny::selectInput(inputId = "par.sous.seq2",label = "Etat 2",choices = "",multiple = FALSE),
                                                                     shiny::selectInput(inputId = "par.sous.seq3",label = "Etat 3",choices = "",multiple = FALSE),
                                                                     shiny::numericInput(inputId = "ligne.suppr", label = "Ligne à supprimer", min = 1, max = 100, value = 1),
                                                                     shiny::actionButton(inputId = "add.button", label = "Ajouter", icon = icon("plus")),
                                                                     br(),br(),
                                                                     shiny::actionButton(inputId = "delete.button", label = "Supprimer", icon = icon("minus"))
                                                                 )
                                                ),
                                                
                                                shiny::selectInput(inputId = "souspop1", label = "Sous Population", choices = "", selected = "", multiple = FALSE),
                                                shiny::uiOutput(outputId= "slider1"),
                                                shiny::uiOutput(outputId= "modalite1"),
                                                conditionalPanel(condition="input.plottype=='flux'",
                                                  shiny::selectInput(inputId = "timeseq1", label = "Pas de temps", choices = "", selected = "", multiple = TRUE, selectize = TRUE),
                                                  
                                                  shiny::actionButton(inputId = "graph1", label = "Afficher le graphique")
                                                  ),
                                                # conditionalPanel(condition="input.plottype=='sous.seq.ch'",
                                                #                  shiny::actionButton(inputId = "graphSousSeq", label = "Afficher le graphique")),
                                                conditionalPanel(condition="input.plottype=='sous.seq'",
                                                                 shiny::sliderInput(inputId = "pmin1", label = "Support minimal",min=0,max=1,value=0.15,step = 0.01)
                                              
                                               )),
                                               column(10,align="center",
                                                      uiOutput("txtAjoutSeq"),
                                                      uiOutput("h4_fluxGlobal"),
                                                      uiOutput("PLOT3")%>% withSpinner(color="#0dc5c1")
                                               
                                                      #,uiOutput("subsTable")
                                                      
                                             
                                               )
                                             )
                                    )
                                  )
                         ),
                         tabPanel("Classification des trajectoires",
                                  tabsetPanel(
                                    #tabsetPanel(
                                    tabPanel(title="Matrice de distance",
                                             fluidRow(
                                               column(3,
                                                      h4("Paramètres généraux de la classification :"),
                                                      shiny::selectInput(inputId = "selection_rows", label = "Sur quelles données voulez-vous travailler?", c("Un echantillon"="Sample", "Des trajectoires uniques avec leurs poids"="unique.traj", "Toutes les trajectoires"="all"), selected = "all", multiple = FALSE),
                                                      shiny::uiOutput("TEXT_NB_UNIQUE_TRAJS"),
                                                      hr(),
                                                      conditionalPanel(condition="input.selection_rows=='Sample'",
                                                                       shiny::numericInput(inputId = "sample_prop", label = "Taille de l'échantillon", value = 0.1, min = 0.05, max = 0.95, step = 0.05),
                                                                       shiny::selectInput(inputId = "sample_var",
                                                                                          label = "Variables utilisées pour la représentativité",
                                                                                          choices = c("Territoire de l'Isère", "Sexe"), multiple = TRUE, selected = NULL)),
                                                      shiny::uiOutput("TEXT_NB_SELECTED_TRAJS") %>% withSpinner(color="#0dc5c1")
                                               ),
                                               column(4,
                                                      h4("Type de distance :"),
                                                      shiny::selectInput(inputId = "type_distance", label = "", c("Edition de trajectoires"="edit", "Attributs communs"="common_attributes", "Distribution d'états"="distrib"), multiple = FALSE),
                                                      hr(),
                                                      conditionalPanel(condition = "input.type_distance=='edit'",
                                                                       h4("Paramètres des coûts :"),
                                                                       selectInput(inputId = "method_edit_cost", label = "method [seqcost(method = )]",
                                                                                   choices = c("CONSTANT" , "TRATE", "FUTURE" , "FEATURES" , "INDELS", "INDELSLOG"),
                                                                                   selected = "TRATE", multiple = FALSE),
                                                                       uiOutput("SEQCOST_INPUTS") %>% withSpinner(color="#0dc5c1"),
                                                                       shiny::actionButton(inputId = "calculCouts", label = "Calcul des couts"))
                                               ),

                                               column(4,
                                                      h4("Paramètres de la matrice de distance :"),
                                                      shiny::selectInput(inputId = "classtype", label = "Quel méthode voulez-vous choisir pour calculer la matrice de distance entre les trajectoires? ", choices = c("OM", "LCS", "HAM"), selected = "OM", multiple = FALSE),
                                                      uiOutput("SEQDIST_INPUTS") %>% withSpinner(color="#0dc5c1"),
                                                      hr(),
                                                      uiOutput("PRINTTIMEDIST") %>% withSpinner(color="#0dc5c1"),
                                                      hr(),
                                                      shiny::actionButton(inputId = "calculDist", label = "Calcul de la matrice de distance"),
                                                      conditionalPanel(condition = "input.calculDist",
                                                      uiOutput("PRINTSEQDIST") %>% withSpinner(color="#0dc5c1")
                                                      )
                                               )
                                             ),
                                             conditionalPanel(condition = "input.type_distance=='edit'",

                                                              fluidRow(
                                                                h3("Affichage des coûts calculés:"),
                                                                hr(),
                                                                h4("Coûts 'indel' :" ),
                                                                uiOutput("PRINTINDEL") %>% withSpinner(color="#0dc5c1"),
                                                                h4("Coûts de substitution :" ),
                                                                uiOutput("PRINTSUBST") %>% withSpinner(color="#0dc5c1")
                                                              )
                                             )
                                             # shiny::conditionalPanel(condition = "input.classtype=='OM'",
                                             #                         shiny::sliderInput(label = "Coûts de substitution: rapport aux coûts indel", inputId = "subst_ratio", min = 0.1, max = 5, step = 0.1, value = 1, width = "80%")
                                             # )

                                    )
                                    ,tabPanel(title="Classification",
                                             fluidRow(
                                               column(4,
                                                      br(),
                                             shiny::selectInput(inputId = "cluster_type", label = "Quel méthode voulez-vous utilsier pour regrouper les séquences ? partir de la matrice de dissemblance?", choices = c("Hierarchical Clustering"="CAH", "FAST Hierarchical Clustering"="fastCAH", "Partitionning Around Medoid"="PAM","Combinaison de la CAH et de PAM"="CAHPAM"), selected = "CAH", multiple = FALSE)),
                                             column(4,br(),
                                             conditionalPanel(condition = "input.cluster_type=='CAH' | input.cluster_type=='CAHPAM'",
                                                              shiny::selectInput(inputId = "agnes_method", choices = c("average", "single", "complete", "ward", "weighted", "flexible", "gaverage"), label = "Choix de la méthode (CAH) :", selected = "ward", multiple = FALSE)),
                                             conditionalPanel(condition = "input.cluster_type=='fastCAH'",
                                                              shiny::selectInput(inputId = "fastclust_method", choices = c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median" ,"centroid"), label = "Choix de la méthode (FAST CAH) :", selected = "ward.D2", multiple = FALSE))
                                             ),
                                             # 
                                             
                                             column(2,
                                                    br(),br(),
                                             shiny::actionButton(inputId = "calculCLUST", label = "Calcul de la classification")
                                    )),
                                    uiOutput("classif"),
                                    uiOutput("tabind"),
                                    fluidRow(
                                      uiOutput("classif_grp"),
                                      column(2,
                                        textOutput("textCluster")
                                      )
                                    )
                                    ),
                                    tabPanel(title="Visualisation des groupes",
                                             
                                             fluidRow(
                                               column(2,
                                                      shiny::selectInput(inputId = "plottypeG", label = "Quel graphique voulez-vous représenter? ", choices = c("d", "f", "I", "ms", "mt", "r","Graphique de flux"="flux","Sous-séquences discriminantes(Pearson)"="Pearson","Sous-séquences choisies (Pearson)"="Pearson.ch"), selected = "d", multiple = FALSE),
                                                          conditionalPanel(condition="input.plottypeG=='Pearson.ch'",
                                                                           wellPanel(shiny::selectInput(inputId = "par.sous.seq1G",label = "Etat 1",choices = "",multiple = FALSE),
                                                                                     shiny::selectInput(inputId = "par.sous.seq2G",label = "Etat 2",choices = "",multiple = FALSE),
                                                                                     shiny::selectInput(inputId = "par.sous.seq3G",label = "Etat 3",choices = "",multiple = FALSE),
                                                                                     shiny::numericInput(inputId = "ligne.supprG", label = "Ligne à supprimer", min = 1, max = 100, value = 1),
                                                                                     shiny::actionButton(inputId = "add.buttonG", label = "Ajouter", icon = icon("plus")),
                                                                                     br(),br(),
                                                                                     shiny::actionButton(inputId = "delete.buttonG", label = "Supprimer", icon = icon("minus"))
                                                                           )
                                                          ),
                                                          shiny::selectInput(inputId = "souspop2", label = "Sous Population", choices = "", selected = "", multiple = FALSE),
                                                          shiny::uiOutput(outputId= "slider2"),
                                                          shiny::uiOutput(outputId= "modalite2"),
                                                      
                                                      conditionalPanel(condition="input.plottypeG=='flux'",
                                                                       shiny::selectInput(inputId = "timeseq2", label = "Pas de temps", choices = "", selected = "", multiple = TRUE, selectize = TRUE),
                                                                       shiny::selectInput(inputId="var_grp",label="Variable Groupe",choices=c(""),selected ="" ,multiple = TRUE),
                                                                       
                                                                       shiny::actionButton(inputId = "graph2", label = "Afficher les graphiques")
                                                      ),
                                                      conditionalPanel(condition="input.plottypeG=='Pearson'",
                                                        shiny::sliderInput(inputId = "pmin",label = "Support minimal (en pourcentage)",min=0,max=1,value=0.15,step = 0.01),
                                                        shiny::numericInput(inputId = "nbAffiche",label = "Nombre d'états affichés",min=1,max=1,value=1,step=1))
                                                      
                                               ),
                                               column(10,align="center",
                                                      
                                                      # shiny::uiOutput("TitreGlobal"),
                                                      # shiny::uiOutput("GraphGlobal"),
                                                      shiny::uiOutput("h4_fluxGrp"),
                                                      shiny::uiOutput("PLOTG")%>% withSpinner(color="#0dc5c1")
                                                      ##,shiny::uiOutput("subsTableG")
                                                      ))
                                             
    
                                        ),
                                    tabPanel(title="Statistiques descriptives")
                                    


                                  ) 
                         )

                         

 )
 )
