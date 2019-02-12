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
load(file="//192.168.1.5/Documents utilisateurs/Documents de Elie/CD_ISERE_RSA/trajs.Rdata")
cpal(seqdata = trajs)<-rev(wesanderson::wes_palette(name = "Darjeeling1", n = length(alphabet(trajs)), type = "discrete"))
read.csv(file = "//192.168.1.5/Documents utilisateurs/Documents de Elie/CD_ISERE_RSA/Gdf.csv", header=TRUE, stringsAsFactors = TRUE)->datas
datas[datas=="Pas de RSA perçu"]<-NA
#### UI ####

ui <- fluidPage(theme = shinytheme("flatly"),
                mainPanel(
                  tabsetPanel(id = "tabpan",
                              tabPanel(title = "Paramètres de la session: ", 
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
                                       
                              ),
                              #tabsetPanel(
                              tabPanel(title="Matrice de distance",
                                       shiny::selectInput(inputId = "classtype", label = "Quel méthode voulez-vous choisir pour calcul la matrice de distance entre les trajectoires? ", choices = c("OM", "LCS", "HAM"), selected = "OM", multiple = FALSE),
                                       shiny::conditionalPanel(condition = "input.classtype=='OM'", 
                                                               shiny::sliderInput(label = "Coûts de substitution: rapport aux coûts indel", inputId = "subst_ratio", min = 0.1, max = 5, step = 0.1, value = 1, width = "80%")
                                       )
                                       
                              ),
                              tabPanel(title="Classification"),
                              tabPanel(title="Visualisation des groupes", 
                                       plotOutput("PLOTG")),
                              tabPanel(title="Statistiques descriptives")
                              
                              #)
                  )
                )
)