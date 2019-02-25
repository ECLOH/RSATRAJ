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
#### DATA ####
data("trajs")
cpal(seqdata = trajs)<-rev(wesanderson::wes_palette(name = "Darjeeling1", n = length(alphabet(trajs)), type = "discrete"))
seqtab(RSATRAJ::trajs[ , ], idxs = 0)->treb
#### UI ####

ui <- shinyUI(navbarPage('RSATRAJ', id="page", collapsible=TRUE, inverse=FALSE,#fluidPage(theme = shinytheme("flatly"),
                tabPanel("Paramètres de la session",
                  #tabsetPanel(id = "tabpan",
                  #            tabPanel(title = "Paramètres de la session: ", 
                                       fileInput(inputId="file1", label="Sélectionnez votre fichier source", multiple = FALSE, accept = NULL, width = NULL),
                                       shiny::dateRangeInput(inputId = "date.range", label = "Dates de début et de fin",
                                                             format = "mm-yyyy"),
                                       shiny::selectInput(inputId = "mode", label = "Mode de travail:", 
                                                          choices = c("Flux d'entrants", "Flux en continu"), 
                                                          selected = c("Flux en continu"), multiple = FALSE),
                                       shiny::numericInput(inputId = "criterNb", label = "Critère de sortie : nombre de mois conécutifs",value = 3, min = 1, max = 36, step = 1),
                                       shiny::actionButton(inputId = "ValidParametres", label = "Je valide ces paramètres"),
                                       textOutput("DATE_RANGE_1")
                              ),
                tabPanel("Statistiques descriptives",
                         tabsetPanel(
                              tabPanel(title = "Taux de transition ( et taux de sortie) ", 
                                       shiny::numericInput(inputId = "PAStrate", label = "Pas de temps pour le calcul des taux de transition",value = 3, min = 1, max = 36, step = 1),
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
                                       shiny::selectInput(inputId = "selection_rows", label = "Sur quelles données voulez-vous travailler?", c("Un echantillon"="Sample", "Des trajectoires uniques avec leurs poids"="unique.traj", "Toutes les trajectoires"="all"), multiple = FALSE),
                                       shiny::uiOutput("TEXT_NB_UNIQUE_TRAJS"),
                                       hr(),
                                       shiny::selectInput(inputId = "type_distance", label = "Type de distance : ", c("Edition de trajectoires"="edit", "Attributs communs"="common_attributes", "Distribution d'états"="distrib"), multiple = FALSE),
                                       shiny::selectInput(inputId = "classtype", label = "Quel méthode voulez-vous choisir pour calculer la matrice de distance entre les trajectoires? ", choices = c("OM", "LCS", "HAM"), selected = "OM", multiple = FALSE),
                                       shiny::conditionalPanel(condition = "input.classtype=='OM'", 
                                                               shiny::sliderInput(label = "Coûts de substitution: rapport aux coûts indel", inputId = "subst_ratio", min = 0.1, max = 5, step = 0.1, value = 1, width = "80%")
                                       )
                                       
                              ),
                              tabPanel(title="Classification"),
                              tabPanel(title="Visualisation des groupes", 
                                       plotOutput("PLOTG")),
                              tabPanel(title="Statistiques descriptives")
                         )
                )
                              
                              #)
                  
                )
)