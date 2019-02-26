#### DATA ####
data("trajs")
cpal(seqdata = trajs)<-rev(wesanderson::wes_palette(name = "Darjeeling1", n = length(alphabet(trajs)), type = "discrete"))
seqtab(RSATRAJ::trajs[ , ], idxs = 0)->treb
#### SERVER ####
server <- function(input, output, session) {
  NB_TRAJS<-shiny::reactive({
    nrow(trajs)
  })
  #
  NB_UNIQUE_TRAJS<-shiny::reactive({
    length(attributes(treb)$row.name)
    })
    renderUI(expr = tags$sub(
      paste("Pour information, il y a",NB_TRAJS(), "trajectoires, et",   NB_UNIQUE_TRAJS(), "trajectoires uniques dans le jeu de données", sep=" "))
      )->output$TEXT_NB_UNIQUE_TRAJS
    #
    NB_SELECT_TRAJS<-shiny::reactive({
      nrow(trajs.forclass())
    })
    renderUI(expr = conditionalPanel(condition = "input.selection_rows == 'Sample'",  
                                     tags$sub(
      paste("Vous avez sélectionné un échantillon de ",NB_SELECT_TRAJS(), "trajectoires", sep=" "))))->output$TEXT_NB_SELECTED_TRAJS
    #
    
  
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
  observe({
    x <- input$type_distance
    names(RSATRAJ::seqdist.args[[x]])->listed_names_methods
    # Can also set the label and select items
    updateSelectInput(session, "classtype",
                      label ="Quel méthode voulez-vous choisir pour calculer la matrice de distance entre les trajectoires? ",
                      choices = listed_names_methods
    )
  })
  #
  shiny::renderUI({
    cost.args[[input$method_edit_cost]]->arg2
    edit.cost.inputs[names(edit.cost.inputs)%in%arg2]
  })->output$SEQCOST_INPUTS
  #
  shiny::renderUI({
    seqdist.args[[input$type_distance]][[input$classtype]]->arg2
    seqdist.inputs[names(seqdist.inputs)%in%arg2]
  })->output$SEQDIST_INPUTS
  #
  trajs.forclass<-reactive({
    if(input$selection_rows=="Sample"){
      sample(x = 1:nrow(trajs), size = 0.2*nrow(trajs), replace = FALSE)->vec.sample
      trajs[vec.sample , ]
    } else {
      trajs
    }
  })
  SEQCOST<-eventReactive(eventExpr = input$calculCouts, {
    seqcost(seqdata=trajs.forclass(), 
            method = input$method_edit_cost, 
            cval = input$subst_ratio, 
            time.varying=input$time_varying_substitution_costs,
            transition=input$transition_substitution_costs,
            lag=input$lag_subst_cost)
  })
  
  output$PRINTINDEL<-renderUI({
    SEQCOST()$indel->the.indels
    if(length(the.indels)>1){
      the.indels<-data.frame("Etats"=alphabet(trajs.forclass()), "Cout(s)_INDEL"=round(the.indels, 2))
      DT::renderDataTable(the.indels)->output$bb
      dataTableOutput("bb")
    } else {
      renderText(as.character(the.indels))->output$bb
      textOutput("bb")
    }
  })
  
  output$PRINTSUBST<-renderUI({
    SEQCOST()$sm->the.sm
    if(class(the.sm)=="matrix"){
      DT::renderDataTable(the.sm)->output$aa
      dataTableOutput("aa")
    } else {
      renderPrint(the.sm)->output$aa
      shiny::verbatimTextOutput ("aa")
    }
  })
  
  SEQDIST<-eventReactive(eventExpr = input$calculDist, {
    seqdist(seqdata = trajs.forclass(), method = input$classtype, refseq = input$refseq_seqdist, norm = input$norm_seqdist, indel = SEQCOST()$indel, sm = SEQCOST()$sm, expcost = input$expcost_seqdist, context=input$context_seqdist)
  })
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