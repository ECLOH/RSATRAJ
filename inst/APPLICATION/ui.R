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

#### UI ####

ui <- shinyUI(navbarPage('RSATRAJ', id="page", collapsible=TRUE, inverse=FALSE,theme=shinytheme("flatly"),#fluidPage(theme = shinytheme("flatly"),
                         tabPanel("Paramètres de la session",
                                  #tabsetPanel(id = "tabpan",
                                  #            tabPanel(title = "Paramètres de la session: ", 
                                  sidebarPanel(fileInput(inputId="file1", label="Sélectionnez votre fichier source", multiple = FALSE, accept = NULL, width = NULL),
                                  shiny::selectInput(inputId="sepcol", label= "Separateur de colonnes", choices=c("Virgule" = ",","Point-Virgule" = ";","Tabulation" = "\t"), selected=","),
                                  shiny::selectInput(inputId="dec", label= "Décimal", choices=c("Virgule" = ",","Point" = "."), selected="."),
                                  shiny::selectInput(inputId="endoding", label= "Encoding ?", choices=c(UTF8 = "UTF-8", Latin1 = "latin1"), selected = "UTF-8", multiple = FALSE, width = "50%"),
                                  shiny::checkboxInput(inputId = "header", label="Header ?",value=FALSE),  
                                    shiny::checkboxInput(inputId = "rowname", label="Rownames ?",value=FALSE),
                                    conditionalPanel(
                                      condition = "input.rowname == true",
                                      shiny::selectInput(inputId="rownames_par", label="Variable rowname", choices = "", multiple = FALSE,selected = NULL, selectize = TRUE)),
                                  shiny::selectInput(inputId = "na", label = "na.strings", choices = c("Vide" , "Espace" = " ", "NA" = "NA"), selected = "NA", multiple = TRUE, selectize = TRUE),  
                                  shiny::selectInput(inputId = "timecol", label = "Variables temporelles", choices = "", selected = "", multiple = TRUE, selectize = TRUE),
                                  
                                  
                                  shiny::dateRangeInput(inputId = "date.range", label = "Dates de début et de fin",
                                                        format = "mm-yyyy"),
                                  shiny::selectInput(inputId = "mode", label = "Mode de travail:", 
                                                     choices = c("Flux d'entrants", "Flux en continu"), 
                                                     selected = c("Flux en continu"), multiple = FALSE),
                                  shiny::numericInput(inputId = "criterNb", label = "Critère de sortie : nombre de mois consécutifs",value = 3, min = 1, max = 36, step = 1),
                                  shiny::actionButton(inputId = "ValidParametres", label = "Je valide ces paramètres"),
                                  #test d'un slider pour la classification
                                  noUiSliderInput(inputId = "sliderTest",label="slider",min=1,max=10,value = c(4,6),limit=2,step=1,margin=2,behaviour = "drag")
                                  #,textOutput("DATE_RANGE_1")
                         )
                         ,
                         mainPanel(
                           shiny::dataTableOutput("contenu"),
                           shiny::verbatimTextOutput("str")
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
                                             shiny::selectInput(inputId = "plottype", label = "Quel graphique voulez-vous représenter? ", choices = c("d", "f", "I", "ms", "mt", "r"), selected = "d", multiple = FALSE),
                                             plotOutput("PLOT3")

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
                                                      shiny::selectInput(inputId = "selection_rows", label = "Sur quelles données voulez-vous travailler?", c("Un echantillon"="Sample", "Des trajectoires uniques avec leurs poids"="unique.traj", "Toutes les trajectoires"="all"), selected = "Sample", multiple = FALSE),
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
                                                                                   selected = "CONSTANT", multiple = FALSE),
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
                                             shiny::selectInput(inputId = "cluster_type", label = "Quel méthode voulez-vous utilsier pour regrouper les séquences ? partir de la matrice de dissemblance?", choices = c("Hierarchical Clustering"="CAH", "FAST Hierarchical Clustering"="fastCAH", "Partitionning Around Medoid"="PAM","Combinaison de la CAH et de PAM"="CAHPAM"), selected = "CAH", multiple = FALSE)),
                                             column(4,
                                             conditionalPanel(condition = "input.cluster_type=='CAH' | input.cluster_type=='CAHPAM'",
                                                              shiny::selectInput(inputId = "agnes_method", choices = c("average", "single", "complete", "ward", "weighted", "flexible", "gaverage"), label = "Choix de la méthode (CAH) :", selected = "ward", multiple = FALSE)),
                                             conditionalPanel(condition = "input.cluster_type=='fastCAH'",
                                                              shiny::selectInput(inputId = "fastclust_method", choices = c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median" ,"centroid"), label = "Choix de la méthode (FAST CAH) :", selected = "ward.D2", multiple = FALSE))),
                                             column(2,
                                             shiny::actionButton(inputId = "calculCLUST", label = "Calcul de la classification"))
                                    ),
                                    uiOutput("DENDOGRAM"),
                                    splitLayout(
                                      
                                    )),
                                    tabPanel(title="Visualisation des groupes",
                                             plotOutput("PLOTG")),
                                    tabPanel(title="Statistiques descriptives"),
                                    
                                    tabPanel(title="Graphique de flux",column(2,
                                            
                                            
                                                                       shiny::selectInput(inputId = "timeseq", label = "Pas de temps", choices = "", selected = "", multiple = TRUE, selectize = TRUE),

                                                                       conditionalPanel(condition="input.tabselected==2 | input.tabselected==3",
                                                                                        selectInput(inputId="var_grp",label="Variable Groupe",choices=c("","acpam4","acpam5","acpam6"),selected ="" ,multiple = FALSE)),
                                                                       conditionalPanel(condition="input.tabselected==2",
                                                                                        selectInput(inputId="id_grp",label="Groupe",choices="",selected = "",multiple = FALSE)),
                                                                       shiny::selectInput(inputId = "souspop", label = "Sous Population", choices = "", selected = "", multiple = FALSE),
                                                                       shiny::uiOutput(outputId= "slider"),
                                                                       shiny::uiOutput(outputId= "modalite"),
                                                                       conditionalPanel(condition="input.tabselected==1",
                                                                                        shiny::actionButton(inputId = "graph1", label = "Afficher le graphique")),
                                                                       conditionalPanel(condition="input.tabselected==2",
                                                                                        shiny::actionButton(inputId = "graph2", label = "Afficher le graphique")),
                                                                       conditionalPanel(condition="input.tabselected==3",
                                                                                        shiny::actionButton(inputId = "graph3", label = "Afficher les graphiques"))
                                                                       
                                                                     ),
                                             mainPanel( tabsetPanel(navbarMenu(title="Flux",
                                                                               tabPanel(title="Global",value=1,fluidPage(
                                                                                 plotOutput(outputId="flux_global") %>% withSpinner(color="#0dc5c1"),
                                                                                 shiny::dataTableOutput("contenuflux")
                                                                                
                                                                               )),
                                                                               tabPanel(title="1 Groupe",value = 2,fluidPage(
                                                                                 plotOutput(outputId="flux_1grp") %>% withSpinner(color="#0dc5c1")
                                                                               )),
                                                                               tabPanel(title="Ensemble",value = 3,fluidPage(
                                                                                 
                                                                                 #shiny::uiOutput(outputId= "flux_ens")
                                                                                 shiny::uiOutput(outputId= "h2_fluxens"),
                                                                                 shiny::plotOutput(outputId= "flux_ens") %>% withSpinner(color="#0dc5c1")
                                                                                 
                                                                                              )
                                                                                        )
                                                                               
                                                                               
                                                                      ),id = "tabselected"))
                                            
                                             )

                                  ) 
                         )

                         

 )
 )
