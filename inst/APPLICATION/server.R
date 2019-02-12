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