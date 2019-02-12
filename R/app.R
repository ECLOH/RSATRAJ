#### PACKAGES ####
library(shiny)
library(TraMineR)
library(ggplot2)
library(TraMineRextras)
library(RColorBrewer)
library(DT)
library(RColorBrewer)
library(wesanderson)
library(shinythemes)

source("//192.168.1.5/Documents utilisateurs/Documents de Elie/CD_ISERE_RSA/FONCTIONS/seqdef_modgap.R", encoding = "UTF-8")
source("//192.168.1.5/Documents utilisateurs/Documents de Elie/CD_ISERE_RSA/FONCTIONS/seqprep_modgap.R", encoding = "UTF-8")
source("//192.168.1.5/Documents utilisateurs/Documents de Elie/CD_ISERE_RSA/FONCTIONS/TraMineR_trunc_modif_for_gap.R", encoding = "UTF-8")
source("//192.168.1.5/Documents utilisateurs/Documents de Elie/CD_ISERE_RSA/FONCTIONS/seqxtract_modgap.R", encoding = "UTF-8")
load(file="//192.168.1.5/Documents utilisateurs/Documents de Elie/CD_ISERE_RSA/trajs.Rdata")
cpal(seqdata = trajs)<-rev(wesanderson::wes_palette(name = "Darjeeling1", n = length(alphabet(trajs)), type = "discrete"))
#### DATAS ####
read.csv(file = "//192.168.1.5/Documents utilisateurs/Documents de Elie/CD_ISERE_RSA/Gdf.csv", header=TRUE, stringsAsFactors = TRUE)->datas
datas[datas=="Pas de RSA perçu"]<-NA
# reshape(datas, direction="long", varying = list(names(datas)[2:37]), idvar = "ID")->datas.l
# datas.l[order(datas.l$ID, datas.l$time) , ]->datas.l
# datas.l$X1.2016[is.na(datas.l$X1.2016)]<-"Pas de RSA perçu"
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
#### SERVER ####
server <- function(input, output, session) {
  DATE_RANGE<-eventReactive(input$ValidParametres, {
    input$date.range
  })
  MOIS_GAP<-eventReactive(input$ValidParametres, {
    input$criterNb
  })
  ####
  PASTRAJ<-reactive({
    input$PAStrate
  })
  TYPETRATE<-reactive({
    input$TYPEtrate
  })
  TRAJTRATE<-reactive({
    seqtrate(seqdata = trajs, lag = PASTRAJ(), time.varying = TYPETRATE())
  })
  LISTTRATE<-reactive({
    if(class(TRAJTRATE())=="array"){
      lapply(seq(dim(TRAJTRATE())[3]), function(x){TRAJTRATE()[ , , x]})->list.trate
    } else {
      list(TRAJTRATE())->list.trate
    }
    list.trate
  })
    observe({
      lapply(1:length(LISTTRATE()), FUN=function(i){
        paste0('TRAJTRATE', i)->id.output
        x<-LISTTRATE()[[i]]
        data.frame(round(x*100, 2))->xx
        gsub(pattern=" ",replacement="_", x = alphabet(trajs))->names.alpha
        names(xx)<-names.alpha
        alphabet(trajs)->xx$DEPART
        xx[, c("DEPART", names.alpha)]->xx
        print(xx)
        output[[id.output]] <- DT::renderDataTable({xx})
    })
    })
    output$infotrate<-renderText({
      paste("Nombre de transitions affichées :", length(LISTTRATE()))
    })
    output$dt <- renderUI({
      tagList(lapply(1:length(LISTTRATE()), function(i) {
        dataTableOutput(paste0('TRAJTRATE', i))
      }))
    })
    TRATELONG<-reactive({
      do.call("rbind", LISTTRATE())->trate.long
      cbind(trate.long, "TIME"=c(matrix(sapply(1:length(LISTTRATE()), function(i){rep(i, length(names.alpha))}), ncol=1)))->trate.long
      data.frame(trate.long)->trate.long
      trate.long$ORIGINE<-row.names(trate.long)
      trate.long %>% gather(-TIME, -ORIGINE, key = APRES, value = value) ->trate.long
      trate.long
    })
    #### Type de graph ####
    pp<-eventReactive(input$plottype, {
      plot3<-seqplot(seqdata = trajs, type = input$plottype)
      plot3
    })
    output$PLOT3 <- renderPlot({
      pp()
    },  
    height = 500, width = 1000)
    #### -> SEQDIST ####
    #seqdist(seqdata = trajs, method = input$classtype)
    ####← PLOT G  ####
    ppG<-eventReactive(input$plottype, {
      plotG<-seqplot(seqdata = trajs, type = "I", group = sample(x = c("H", "F"), size = 27000, prob = c(0.4, 0.6), replace = TRUE))
      plotG
    })
    output$PLOTG <- renderPlot({
      ppG()
    },  
    height = 500, width = 1000)
}

# Run the application 
shinyApp(ui = ui, server = server)

