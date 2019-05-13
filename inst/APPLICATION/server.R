#### DATA ####
#data("trajs")
#trajs<-read.csv("G:/dgsd/Dopro/Projets/2018/E - Etude_RSA/E-RSA_Appli_Shiny_Trajectoires/GitHub/RSATRAJ-master/data/trajs.csv",row.names = 1)

# rev(wesanderson::wes_palette(name = "Darjeeling1", n = length(alphabet(trajs)), type = "discrete"))->cpal.seq
# cpal(seqdata = trajs)<-cpal.seq
# seqtab(trajs[ , ], idxs = 0, format = "STS")->unique.trajs

#### SERVER ####
server <- function(input, output, session) {
  #### Chargement des données ####
 
      trajs <- reactiveValues(df = NULL, dataSource = NULL)
               
  observe({
    req(input$file1)
    trajs$dataSource <- input$file1$datapath
    updateCheckboxInput(session=session,inputId = "rowname",value = FALSE )
    updateSelectInput(session = session, inputId = "rownames_par",choices = "")
  })
  
  observe({
    req(input$sepcol)
    updateCheckboxInput(session=session,inputId = "rowname",value = FALSE )
    updateSelectInput(session = session, inputId = "rownames_par",choices = "")
  })
  

  argna<-reactive({
    req(trajs$dataSource)
    if ("Vide" %in% input$na){
      c("",input$na)
      
    }else{
      input$na
      
    }
  })
#### Chargement premier fichier de données ####
  #### OBJET .RData ####
  list_csv<-reactive({
    if(input$DataType=="objet"){
        if ( is.null(input$objetFile)) return(NULL)
        inFile <- input$objetFile
        file <- inFile$datapath
        # load the file into new environment and get it from there
        e = new.env()
        name <- load(file, envir = e)
        data <- e[[name]]
        #
        mycolumns<-unique(unlist(Reduce(intersect,list(lapply(X = data, FUN = names))), 
                                 use.names = FALSE))
        updateSelectInput(session = session, inputId = "timecol", choices = mycolumns)
        #
        return(data)
      }
  })
  renderPrint(print(length(list_csv())))->output$CONTROLDATA
 #### Un fichier source ####
  data<-reactive({
    req(trajs$dataSource)
    if(input$DataType=="fichier"){ #### Chargement d'un seul fichier CSV ####

    if (input$rowname==TRUE && input$rownames_par=="")
    {
      # userData <- read.csv(file = input$file1$datapath, sep = input$sepcol, encoding = input$endoding)
      # trajs$df<- userData
      colonneID<- c(colnames(trajs$df)[colId(df = trajs$df)])
      updateSelectInput(session = session, inputId = "rownames_par",choices = c("",colonneID))
      }
    if (input$rowname==TRUE && input$rownames_par!=""){
      userData <- read.csv(file = input$file1$datapath, 
                           sep = input$sepcol, 
                           encoding = input$endoding,
                           row.names = input$rownames_par,
                           header=input$header,na.strings = argna(),
                           dec=input$dec)
      mycolumns<-c(colnames(userData))
      updateSelectInput(session = session, inputId = "timecol", choices = mycolumns)
      trajs$df<- userData

    }

    if(input$rowname==FALSE){
      userData <- read.csv(file = input$file1$datapath, sep = input$sepcol, encoding = input$endoding,header=input$header,na.strings = argna(),dec=input$dec)
      mycolumns <- c(colnames(userData))
      updateSelectInput(session = session, inputId = "timecol", choices = mycolumns)
      trajs$df<- userData}
    return(trajs$df)
    } else {
        
      }
  })
  output$contenu<-shiny::renderDataTable({
    req(data())
    data()
  })
  
  #### Paramétrage des trajectoires ####
  output$DATA_UI<-shiny::renderUI({
    if(input$DataType=="fichier"){
      shiny::dateRangeInput(inputId = "date.range", label = "Dates de début et de fin",
                            format = "mm-yyyy")->the.ui
    } else {
      if(input$DataType=="objet"){
        names(list_csv())->names.pick
        list(
          shiny::selectInput(inputId = "PICKDATE1", label = "Debut:",
                           choices = names.pick, multiple = FALSE),
          shiny::selectInput(inputId = "PICKDATE1", label = "Fin:",
                             choices = names.pick, multiple = FALSE))->the.ui
      } else {h3("error")->the.ui}
    }
    the.ui
      })
  data.seq<-eventReactive(eventExpr = input$ValidParametres, {
  req(data())
    if(input$DataType=="fichier"){

  if (length(input$timecol)<2){
    showModal(modalDialog(
      title = "Important message",
      "Il faut mettre au moins deux variables temporelles.",
      easyClose = TRUE
    ))
  }
  else if ((length(input$timecol)-1)<input$PAStrate){
    showModal(modalDialog(
      title = "Important message",
      "Il faut que le 'Pas de temps pour le calcul des taux de transition' dans l'onglet Statistiques descriptives/taux de transition (et taux de sortie) soit inférieur strictement au nombre de variables temporelles.",
      easyClose = TRUE
    ))
  }
  else   {
    # updateNumericInput(session=session, inputId = "PAStrate",value=1)
    s<-seqdef(data()[,input$timecol],cpal = NULL)
    
    if (length(alphabet(s))<=12){
      #permet d'avoir les mêmes couleurs que pour les graphiques de flux
      a<-col_flux(data = data(),seq.data = s)
      attr(s, "cpal") <- unname(a[alphabet(s)])
    }
    
    return(s)
  } 
    } else {
      if(input$DataType=="objet"){
      }
    }
  })



observeEvent(eventExpr = data.seq(),{
  req(data.seq())
  updateNumericInput(session=session, inputId = "PAStrate",max=length(input$timecol)-1)
})


  ### NOMBRE DE TRAJECTOIRES: TOTAL ET  SELECTIONNEES  ####
  NB_TRAJS<-shiny::reactive({
    req(data.seq())
    nrow(data.seq())
  })
  unique.trajs<-shiny::reactive({
    req(data.seq())
    seqtab(data.seq()[ , ], idxs = 0, format = "STS")
    })

    NB_UNIQUE_TRAJS<-shiny::reactive({
    length(attributes(unique.trajs())$row.name)
    })
   

    
    renderUI(expr = tags$sub(
      paste("Pour information, il y a",NB_TRAJS(), "trajectoires, et",   NB_UNIQUE_TRAJS(), "trajectoires uniques dans le jeu de données",sep=" "))
      )->output$TEXT_NB_UNIQUE_TRAJS

  NB_SELECT_TRAJS<-shiny::reactive({
    nrow(trajs.forclass())
  })


    renderUI(expr = #conditionalPanel(condition = "input.selection_rows == 'Sample'",
                                     tags$strong(
                                       if(input$selection_rows == "Sample"){
      paste("Vous avez sélectionné un échantillon de ",NB_SELECT_TRAJS(), "trajectoires", sep=" ")
                                       } else {if(input$selection_rows == "unique.traj"){
                                         paste("Vous avez sélectionné",NB_SELECT_TRAJS(), "trajectoires uniques pondérées", sep=" ")
                                         } else {
                                           paste("Vous avez tout sélectionné, soit ",NB_SELECT_TRAJS(), "trajectoires", sep=" ")
                                         }
                                       }
                                         ))->output$TEXT_NB_SELECTED_TRAJS

    
  DATE_RANGE<-eventReactive(input$ValidParametres, {
    input$date.range
  })
  MOIS_GAP<-eventReactive(input$ValidParametres, {
    input$criterNb
  })
# 
#    ####
  
  
   PASTRAJ<-reactive({
     input$PAStrate
   })
   TYPETRATE<-reactive({
     input$TYPEtrate
   })
   TRAJTRATE<-reactive({
     req(data.seq())
     seqtrate(seqdata = data.seq(), lag = PASTRAJ(), time.varying = TYPETRATE())
   })
   LISTTRATE<-reactive({
     req(TRAJTRATE())
     if(class(TRAJTRATE())=="array"){
       lapply(seq(dim(TRAJTRATE())[3]), function(x){TRAJTRATE()[ , , x]})->list.trate
     } else {
       list(TRAJTRATE())->list.trate
     }
     list.trate
   })
   observe({
     req(data.seq(),LISTTRATE())
     lapply(1:length(LISTTRATE()), FUN=function(i){
       paste0('TRAJTRATE', i)->id.output
       x<-LISTTRATE()[[i]]
       data.frame(round(x*100, 2))->xx
       gsub(pattern=" ",replacement="_", x = alphabet(data.seq()))->names.alpha
       names(xx)<-names.alpha
       alphabet(data.seq())->xx$DEPART
       xx[, c("DEPART", names.alpha)]->xx
       print(xx)
       output[[id.output]] <- DT::renderDataTable({xx})
     })
   })
   output$infotrate<-renderText({
     req(LISTTRATE())
     paste("Nombre de transitions affichées :", length(LISTTRATE()))
   })
   output$dt <- renderUI({
     req(LISTTRATE())
     tagList(lapply(1:length(LISTTRATE()), function(i) {
       dataTableOutput(paste0('TRAJTRATE', i))
     }))
   })
   TRATELONG<-reactive({
     req(LISTTRATE())
     do.call("rbind", LISTTRATE())->trate.long
     cbind(trate.long, "TIME"=c(matrix(sapply(1:length(LISTTRATE()), function(i){rep(i, length(names.alpha))}), ncol=1)))->trate.long
     data.frame(trate.long)->trate.long
     trate.long$ORIGINE<-row.names(trate.long)
     trate.long %>% gather(-TIME, -ORIGINE, key = APRES, value = value) ->trate.long
     trate.long
   })
   #### Type de graph ####

   observeEvent(eventExpr = input$ValidParametres,{
     req(data.seq())
     updateSelectInput(session = session, inputId = "timeseq1", choices = input$timecol)
     
     colsouspop<-colnames(data())[!(colnames(data()) %in% input$timecol)]
     updateSelectInput(session = session, inputId = "souspop1", choices = c("Aucune",colsouspop))
     
   })
   
   observeEvent(input$souspop1,{
     req(input$souspop1)
     if (input$souspop1=="Aucune"){
       updateSelectInput(session = session, inputId = "souspop_modalite1", choices = "" )
     }
   })
   
   output$slider1<- renderUI({
     if(input$souspop1!="Aucune"){
       if (is.numeric(data()[,input$souspop1])){
         min<-min(data()[,input$souspop1],na.rm = TRUE)
         max<-max(data()[,input$souspop1],na.rm = TRUE)
         sliderInput(inputId = "sous_pop_num1", label="Slider",min=min,max=max,value = c(min,max))
       }
     }
   })
   output$modalite1<- renderUI({
     if(input$souspop1!="Aucune"){
       if (is.factor(data()[,input$souspop1])){
         selectInput(inputId = "souspop_modalite1",label="Modalité", choices = levels(data()[,input$souspop1]),selected="",multiple = TRUE)
       }
     }
   })
   
   #### Selection des sous populations ####
   data.select1<-reactive({
     req(input$souspop1)
     
       if (input$souspop1=="Aucune" || input$souspop1==""){
         data.select<-data()
       }else{

         if (is.factor(data()[,input$souspop1])){
           req(input$souspop_modalite1)
           data.select<-data()[(data()[,input$souspop1] %in% c(input$souspop_modalite1)),]
         }
         if (is.numeric(data()[,input$souspop1])){
           req(input$sous_pop_num1)
           data.select<-data()[which(data()[,input$souspop1]<= max(input$sous_pop_num1,na.rm=TRUE) & data()[,input$souspop1]>= min(input$sous_pop_num1,na.rm=TRUE)),]
           
         }
         
       }
       return(data.select)
     
   })
   
   seq.select1<-reactive({
     req(input$souspop1)
     
       if (input$souspop1=="Aucune" || input$souspop1==""){
         seq.select<-data.seq()
       }else{

         if (is.factor(data()[,input$souspop1])){
           req(input$souspop_modalite1)
           seq.select<-data.seq()[(data()[,input$souspop1] %in% c(input$souspop_modalite1)),]
         }
         if (is.numeric(data()[,input$souspop1])){
           req(input$sous_pop_num1)
           seq.select<-data.seq()[which(data()[,input$souspop1]<= max(input$sous_pop_num1,na.rm=TRUE) & data()[,input$souspop1]>= min(input$sous_pop_num1,na.rm=TRUE)),]

         }
       }

       return(seq.select)
     
   })

   
   
   col_periode1<-eventReactive(eventExpr = length(input$timeseq1)>=2,{
     input$timeseq1
   })
   
   flux1<-eventReactive(eventExpr = input$graph1,{
     req(data.select1(),col_periode1(),seq.select1())
     if (input$souspop1!="Aucune" && is.factor(data()[,input$souspop1])) {
       lapply(1:length(input$souspop_modalite1), FUN=function(i){
          graph_flux_grp(data=data.select1(),seq_data=seq.select1(),col_periode=col_periode1(),var_grp=input$souspop1,label_grp= as.character(input$souspop_modalite1[i]))
       })
     }
     else{
        return(list(graph_flux(data=data.select1(),seq_data=seq.select1(),col_periode=col_periode1())))
     }
     
   })
   
   #### Graphique sous séquence ####
   event.seqGlobal<-reactive({
     if (req(input$plottype) %in% c("sous.seq","sous.seq.ch")){
       req(seq.select1())
       return(seqecreate(seq.select1()[order(row.names(seq.select1())), ], tevent="state", use.labels=FALSE))
     }
  })
   
   subsGlobal<-reactive({
     if (req(input$plottype) == "sous.seq"){
       req(event.seqGlobal())
       return(seqefsub(event.seqGlobal(),pmin.support=input$pmin1))
     }
     if (req(input$plottype) == "sous.seq.ch"){
       req(event.seqGlobal(),values$df)
       if(nrow(values$df)>0){
         
         vectSeq<-vect.sous.seq(data = values$df)
         seqefsub(event.seqGlobal(),str.subseq=vectSeq)->p
         return(p[order(p$data$Support,decreasing = TRUE),])
       }
         
     }
   })
   
   output$txtAjoutSeq<-renderUI({
     if (req(input$plottype) == "sous.seq.ch"){
       if(!(nrow(values$df)>0)){
         output$txtAjout<-renderText({
           return("Ajouter une séquence en choissant une succession d'état et en appuyant sur ajouter")
         })
         return(textOutput("txtAjout"))
       }
     }
   })
   
   # output$subsTable<-renderUI({
   #   if (req(input$plottype) %in% c("sous.seq","sous.seq.ch")){
   #       output$tableSubs<-renderDataTable({
   #           req(subsGlobal())
   #           sousSeqData<-cbind(as.character(subsGlobal()$subseq),subsGlobal()$data)
   #           
   #           names(sousSeqData)<-c("Sous-séquences","Support","Count")
   #           if(req(input$plottype) == "sous.seq.ch"){
   #             
   #             rownames(sousSeqData)<-(1:nrow(sousSeqData))
   #           }
   #           return(sousSeqData)
   #      })
   #       return(dataTableOutput("tableSubs"))
   #   }    
   # })
   
   ##### Graphique sous-séquences choisies #####
      ### Mise a jour des inputs permettant de choisir des états ###
   observe({
     input$plottype
     isolate({
       if (req(input$plottype)=="sous.seq.ch"){
          req(seq.select1())
         updateSelectInput(session = session,inputId = "par.sous.seq1",choices = alphabet(seq.select1()))
         updateSelectInput(session = session,inputId = "par.sous.seq2",choices = alphabet(seq.select1()))
         updateSelectInput(session = session,inputId = "par.sous.seq3",choices = cbind("Aucun",alphabet(seq.select1())))
       }
     })
   })
   
   observe({
     updateNumericInput(session = session,inputId = "ligne.suppr",max=nrow(values$df))
   })
   
   values <- reactiveValues()
   values$df <-  as.data.frame(setNames(replicate(3,character(0), simplify = F),c("Etat1","Etat2","Etat3") ))
   
   observeEvent(input$add.button,{
     req(input$par.sous.seq1,input$par.sous.seq2)
     newRow <- data.frame(input$par.sous.seq1, input$par.sous.seq2,input$par.sous.seq3)
     colnames(newRow)<-colnames(values$df)
     values$df <- rbind(values$df,newRow)
     rownames(values$df)<-(1:nrow(values$df))
   })
   
   observeEvent(input$delete.button,{
     if(nrow(values$df)>1){
      values$df[!(vect.sous.seq(values$df) %in% as.character(subsGlobal()$subseq)[input$ligne.suppr]),]->values$df
       rownames(values$df)<-(1:nrow(values$df))
     }else {
       values$df <- values$df[-nrow(values$df), ]
     }
   })  
   observe({
     req(values$df)
     values$df<-unique(values$df)
   })
   

   
        #### Graphiques ####
   
   ordre1<-reactive({
     if(req(input$plottype)=="flux") {
       input$graph1
       isolate({
         if (input$souspop1!="Aucune" && is.factor(data()[,input$souspop1])) {
           req(input$souspop_modalite1)     
           return(taille_graph_flux(length(input$souspop_modalite1)))
         }else {
           return(cbind(1,2))
         }
       })
     }
       if(req(input$plottype) %in% c("d", "f", "I", "ms", "mt", "r")) {
         if (input$souspop1!="Aucune" && is.factor(data()[,input$souspop1])) {
           req(input$souspop_modalite1)     
           return(taille_graph_flux(length(input$souspop_modalite1)))
         }else {
           return(cbind(1,2))
         }
       }
   })
   
   haut1<-function(){
     ordre3<-ordre1()
     return(dim(ordre3)[1]*400)}
   
   output$PLOT3<- renderUI({
     if (req(input$plottype) %in% c("sous.seq","sous.seq.ch")){
       output$PLOT<-renderPlot({
           req(subsGlobal())
           return(graph_sous_sequences(subsGlobal()))
           #return( plot(subsGlobal(),ylim=c(0,1),main = "Graphique des sous-séquences selon leur support") )
       },height = function() {
         session$clientData$output_PLOT_width
       })
       return(plotOutput("PLOT"))
     }
     if (req(input$plottype) == "flux"){
       output$PLOT<-renderPlot({
         req(flux1(),ordre1())
           return(marrangeGrob(flux1(), layout_matrix=ordre1()))
       },width = 1300,height = haut1)
       return(plotOutput("PLOT")%>% withSpinner(color="#0dc5c1"))
     }
     if (req(input$plottype) %in% c("d", "f", "I", "ms", "mt", "r")) {
       output$PLOT<-renderPlot({
         req(ordre1())
           if (input$souspop1!="Aucune" && is.factor(data()[,input$souspop1])){
             return(seqplot(seqdata = seq.select1(), type = input$plottype, group = data.select1()[,input$souspop1]))
           }
           else{
              return(seqplot(seqdata = seq.select1(), type = input$plottype))
           }
      },width = 1300,height = haut1)
       return(plotOutput("PLOT"))
     }
     
   })
  
   ### Titre rappelant la selection choisie #####
        reactive({
       if(input$plottype=="flux"){
         input$graph1
         isolate({
         if (input$souspop1=="Aucune" || input$souspop1==""){
           return("Vous avez sélectionné auncune sous population")
         }else{
           
           if (is.factor(data()[,input$souspop1])){
             req(input$souspop_modalite1)
             return(paste("Vous avez sélectionné la sous population",input$souspop1, "avec les modalités",paste(input$souspop_modalite1,collapse = ", ")))
           }
           if (is.numeric(data()[,input$souspop1])){
             
             return(paste("Vous avez sélectionné la sous population",input$souspop1, "entre",min(input$sous_pop_num1,na.rm=TRUE),"et",max(input$sous_pop_num1,na.rm=TRUE)))
             
           }
         }
        })
         
       } else {
         if (input$souspop1=="Aucune" || input$souspop1==""){
           return("Vous avez sélectionné auncune sous population")
         }else{
           
           if (is.factor(data()[,input$souspop1])){
             req(input$souspop_modalite1)
             return(paste("Vous avez sélectionné la sous population",input$souspop1, "avec les modalités",paste(input$souspop_modalite1,collapse = ", ")))
           }
           if (is.numeric(data()[,input$souspop1])){
             
             return(paste("Vous avez sélectionné la sous population",input$souspop1, "entre",min(input$sous_pop_num1,na.rm=TRUE),"et",max(input$sous_pop_num1,na.rm=TRUE)))
             
           }
         }
       }
     
   })->text1
   
   renderUI({
     req(text1())
         renderText(text1())->output$textGlobal
         return(h4(textOutput("textGlobal")))
   })->output$h4_fluxGlobal
   
   #### UPDATE "classtype" input selon input$type_distance ####
  observe({
    x <- input$type_distance
    names(
     seqdist.args[[x]])->listed_names_methods
    # Can also set the label and select items
    updateSelectInput(session, "classtype",
                      label ="Quel méthode voulez-vous choisir pour calculer la matrice de distance entre les trajectoires? ",
                      choices = listed_names_methods
    )
  })
  #### SEQCOST and SEQDIST input ####
  observeEvent(eventExpr = input$method_edit_cost, {
    shiny::renderUI({
    cost.args[[input$method_edit_cost]]->arg2
    edit.cost.inputs[names(edit.cost.inputs)%in%arg2]
  })->output$SEQCOST_INPUTS
  })
  #
  shiny::renderUI({
    seqdist.args[[input$type_distance]][[input$classtype]]->arg2
    seqdist.inputs[names(seqdist.inputs)%in%arg2]
  })->output$SEQDIST_INPUTS
  ###trajs.forclass  ####
  trajs.forclass<-reactive({#shiny::eventReactive(eventExpr = input$selection_rows, {
    req(data.seq())
    if(input$selection_rows=="Sample"){
      ###sample ####
      sample(x = 1:nrow(data.seq()), size = input$sample_prop*nrow(data.seq()), replace = FALSE)->vec.sample
      data.seq()[vec.sample , ]
    } else {
      if(input$selection_rows=="unique.traj"){
        #### unique.traj ####

        rev(wesanderson::wes_palette(name = "Darjeeling1", n = length(alphabet(data.seq())), type = "continuous"))->cpal.seq
        #cpal(seqdata = data.seq())<-cpal.seq
        #seqtab(data.seq()[ , ], idxs = 0, format = "STS")->unique.trajs
        #data.frame(unique.trajs)->unique.trajs.df
        data.frame(unique.trajs())->unique.trajs.df
        seqdef(data = unique.trajs.df, weights = attributes(unique.trajs())$freq$Percent, cpal = cpal.seq)->unique.trajs.seq
        unique.trajs.seq
      } else {
        #### all ####
        data.seq()
      }
    }
  })
#   ### SEQCOST  ###
#
   SEQCOST<-eventReactive(eventExpr = input$calculCouts, {
     req(trajs.forclass())
     seqcost(seqdata=trajs.forclass(),
             method = input$method_edit_cost,
             cval = input$subst_ratio,
             time.varying=input$time_varying_substitution_costs,
             transition=input$transition_substitution_costs,
             lag=input$lag_subst_cost, weighted = TRUE)
   })
#
#    #### PRINT COSTS ####
   output$PRINTINDEL<-renderUI({
     req(SEQCOST())
     SEQCOST()$indel->the.indels
     if(length(the.indels)>1){
       the.indels<-data.frame("Etats"=alphabet(trajs.forclass()), "Cout(s)_INDEL"=round(the.indels, 2))
       DT::renderDataTable(the.indels)->output$bb
       dataTableOutput("bb", width = "80%")
     } else {
       renderText(as.character(the.indels))->output$bb
       textOutput("bb")
     }
   })
#
   output$PRINTSUBST<-renderUI({
     req(SEQCOST())
     SEQCOST()$sm->the.sm
     if(class(the.sm)=="matrix"){
       DT::renderDataTable(the.sm)->output$aa
       dataTableOutput("aa")
     } else {
       renderPrint(the.sm)->output$aa
       shiny::verbatimTextOutput ("aa")
     }
   })
# #
   #### SEQDIST  ####
   output$PRINTTIMEDIST<-renderUI({
     req(NB_SELECT_TRAJS())
     predict.time.dist(nb.sequences = NB_SELECT_TRAJS())->pred.data
     paste("Pour", NB_SELECT_TRAJS(), "trajectoires, le temps de calcul estimé pour la fonction seqdist() est de", pred.data$nb.secondes, "secondes, soit", pred.data$nb.minutes, "minutes")->thetext
       renderText(thetext)->output$thetext
       textOutput("thetext")
   })
   SEQDIST<-eventReactive(eventExpr = input$calculDist, {
     req(input$refseq_seqdist,trajs.forclass(),SEQCOST(),input$norm_seqdist)
     if(input$refseq_seqdist==FALSE){REFSEQ<-NULL} else {REFSEQ<-input$refseq_seqdist==FALSE}
     seqdist(seqdata = trajs.forclass(), method = input$classtype, refseq = REFSEQ, norm = input$norm_seqdist, indel = SEQCOST()$indel, sm = SEQCOST()$sm, expcost = input$expcost_seqdist, context=input$context_seqdist, weighted = TRUE)
   })
#
   output$PRINTSEQDIST<-renderUI({
     req(SEQDIST())
 renderText(paste("Création d'un objet 'dist' comportant", length(SEQDIST()), "élements. /n La distance minimale est de", min(SEQDIST()), "la distance maximale de", max(SEQDIST()), "et la distance moyenne de", round(sum(SEQDIST())/length(SEQDIST()), 2), sep = " "))->output$cc
     textOutput("cc") %>% withSpinner(color="#0dc5c1")
   })
   ### CLASSIFICATION ####
   SEQCLASS<-eventReactive(eventExpr = input$calculCLUST, {
     req(SEQDIST())
     if(input$cluster_type=="CAH" | input$cluster_type=="CAHPAM"){
       #agnes(x = SEQDIST(), method = input$agnes_method)
       agnes(as.dist(SEQDIST()), method = input$agnes_method, keep.diss=FALSE)
     } else {
       if(input$cluster_type=="fastCAH"){
         fastcluster::hclust(d = SEQDIST() , method = input$fastclust_method, members = NULL)

       }
     }
   })
   
     ##### Graphiques #####
   output$classif<- renderUI({
     req(SEQCLASS())
     input$calculCLUST
     isolate({
     if (input$cluster_type=="CAH" | input$cluster_type=="fastCAH"){
       renderPlot(plot(SEQCLASS(), which.plots = 2))->output$dd
       return(plotOutput("dd") %>% withSpinner(color="#0dc5c1"))
     }
       if(input$cluster_type=="CAHPAM"){
         output$dendo<-renderPlot({
             plot(as.dendrogram(SEQCLASS()))
           })
         output$inertie<-renderPlot({
             plot(sort(SEQCLASS()$height, decreasing=TRUE)[1:20], type="s", xlab="nb de classes", ylab="inertie")
           })
         
           
         return(tagList(
           noUiSliderInput(inputId = "SliderGrp",label="Nombre de groupes",min=2,max=10,value = c(4,6),limit=2,step=1,margin=2,behaviour = "drag"),
           fluidRow(column(12,
                    splitLayout(
           plotOutput("dendo") %>% withSpinner(color="#0dc5c1"),
           plotOutput("inertie") %>% withSpinner(color="#0dc5c1"))))
           
         ))
         
       }
     })
     
   })
  
   # output$DENDOGRAM<-renderUI({
   #   req(SEQCLASS())
   #   renderPlot(plot(SEQCLASS(), which.plots = 2))->output$dd
   #   plotOutput("dd") %>% withSpinner(color="#0dc5c1")
   # })

   
          #### Tableau indicateurs ####
   
   output$tabind<-renderUI({
     req(SEQCLASS(),input$SliderGrp)
     input$calculCLUST
     input$SliderGrp
     isolate({
       if(input$cluster_type=="CAHPAM"){
         indicateur<-Creation_indicateur(nb_cluster_min=min(input$SliderGrp,na.rm=TRUE),nb_cluster_max=max(input$SliderGrp,na.rm=TRUE),mat_dist=SEQDIST(),intialclust=SEQCLASS())
         output$tabIndicateur<-renderFormattable(tableau_cluster(indicateurs=indicateur$dataFrame))
         return(fluidRow(column(12,
                         formattableOutput("tabIndicateur"))))
       }
         
         })
   })
   

   
   output$classif_grp<-renderUI({
     req(SEQCLASS(),input$SliderGrp)
     input$calculCLUST
     input$SliderGrp
     isolate({
       if (input$cluster_type=="CAHPAM"){
         return(tagList(
           
           column(4,
                  shiny::numericInput(inputId = "nb_cluster",label="Nombre de groupes choisi",step=1,value = min(input$SliderGrp,na.rm=TRUE)+1,min=min(input$SliderGrp,na.rm=TRUE),max=max(input$SliderGrp,na.rm=TRUE))
           ),
           column(2,
                  shiny::actionButton(inputId = "Bouton_Clustering",label = "Faire les groupes")
           )
           
         ))
         
       }
     })
   })
   
   
   dataCluster<-eventReactive(eventExpr = input$Bouton_Clustering,{
     indicateur<-Creation_indicateur(nb_cluster_min=min(input$SliderGrp,na.rm=TRUE),nb_cluster_max=max(input$SliderGrp,na.rm=TRUE),mat_dist=SEQDIST(),intialclust=SEQCLASS())
     return(data_cluster(indicateur,data(),input$nb_cluster))
   })
   
   output$textCluster<-renderText({
     req(dataCluster())
     
     input$Bouton_Clustering
     isolate({
       return(paste("Vous avez créé",input$nb_cluster,"groupes"))
     })
     
   })
   
#
   ### PLOT G  ####
        ##### Mise a jour des inputs #####
   observeEvent(eventExpr = input$Bouton_Clustering,{
     updateSelectInput(session = session, inputId = "timeseq2", choices = input$timecol)
     
     colsouspop2<-colnames(data())[!(colnames(data()) %in% input$timecol)]
     updateSelectInput(session = session, inputId = "souspop2", choices = c("Aucune",colsouspop2))
   })
   
   observeEvent(input$souspop2,{
     req(input$souspop2)
     if (input$souspop2=="Aucune"){
       updateSelectInput(session = session, inputId = "souspop_modalite2", choices = "" )
     }
   })
   
   output$slider2<- renderUI({
     if(input$souspop2!="Aucune"){
       if (is.numeric(data()[,input$souspop2])){
         min<-min(data()[,input$souspop2],na.rm = TRUE)
         max<-max(data()[,input$souspop2],na.rm = TRUE)
         sliderInput(inputId = "sous_pop_num2", label="Slider",min=min,max=max,value = c(min,max))
       }
     }
   })
   output$modalite2<- renderUI({
     if(input$souspop2!="Aucune"){
       if (is.factor(data()[,input$souspop2])){
         selectInput(inputId = "souspop_modalite2",label="Modalité", choices = levels(data()[,input$souspop2]),selected="",multiple = TRUE)
       }
     }
   })
   
   grp<-reactive({
     #if (input$plottypeG=="flux"){
       input$souspop_modalite2
       input$souspop2
       input$sous_pop_num2
       isolate({
         req(data.select2())
         unique(data.select2()[,"Clustering"])
       })
     # }else{
     #   req(dataCluster())
     #   unique(dataCluster()[,"Clustering"])
     # }
   })

   observe({
     req(grp())
     updateSelectInput(session = session, inputId = "var_grp", choices = grp())
   })
   
      #### Selection de la sous population ####
   data.select2<-reactive({
     req(input$souspop2,dataCluster())

     #if (req(input$plottypeG) == "flux"){
       if (input$souspop2=="Aucune" || input$souspop2==""){
         data.selectG<-dataCluster()
       }else{
         
         if (is.factor(dataCluster()[,input$souspop2])){
           req(input$souspop_modalite2)
           data.selectG<-dataCluster()[(dataCluster()[,input$souspop2] %in% c(input$souspop_modalite2)),]
         }
         if (is.numeric(dataCluster()[,input$souspop2])){
           req(input$sous_pop_num2)
           data.selectG<-dataCluster()[which(dataCluster()[,input$souspop2]<= max(input$sous_pop_num2,na.rm=TRUE) & dataCluster()[,input$souspop2]>= min(input$sous_pop_num2,na.rm=TRUE)),]
           
         }
         
       }
       return(data.selectG)
     #}
   })
   
   seq.select2<-reactive({
     req(input$souspop2)
     
     if (input$souspop2=="Aucune" || input$souspop2==""){
       seq.selectG<-data.seq()
     }else{
       
       if (is.factor(data()[,input$souspop2])){
         req(input$souspop_modalite2)
         seq.selectG<-data.seq()[(data()[,input$souspop2] %in% c(input$souspop_modalite2)),]
       }
       if (is.numeric(data()[,input$souspop2])){
         req(input$sous_pop_num2)
         seq.selectG<-data.seq()[which(data()[,input$souspop2]<= max(input$sous_pop_num2,na.rm=TRUE) & data()[,input$souspop2]>= min(input$sous_pop_num2,na.rm=TRUE)),]
         
       }
     }
     
     return(seq.selectG)
     
   })
   
   ### Selectionne de la période temporelle ###
   col_periode2<-eventReactive(eventExpr = length(input$timeseq2)>=2,{
     input$timeseq2
   })
   
   

   ### Graphique de flux pour les groupes ###
   
   flux3<-eventReactive(eventExpr = input$graph2,{
     
          req(data.select2(),seq.select2(),col_periode2(),grp())

          lapply(1:length(input$var_grp), FUN=function(i){
            graph_flux_grp(data = data.select2(),seq_data = seq.select2(),col_periode2(),var_grp = "Clustering",label_grp = as.character(input$var_grp[i]))

          })
        
     })
   

       
     ###### Graphique coefficient de Pearson ######
     
     event.seq<-reactive({
       if (req(input$plottypeG) %in% c("Pearson","Pearson.ch")){
          req(data.seq(),dataCluster())
          return(seqecreate(seq.select2()[order(row.names(seq.select2())), ], tevent="state", use.labels=FALSE))
       }
     })
     
     subs<-reactive({
       if (req(input$plottypeG) == "Pearson"){
          req(event.seq())
          return(seqefsub(event.seq(),pmin.support=input$pmin))
       }
       if (req(input$plottypeG) == "Pearson.ch"){
         req(event.seq(),valuesG$df)
         if(nrow(valuesG$df)>0){
           
           vectSeqG<-vect.sous.seq(data = valuesG$df)
           seqefsub(event.seq(),str.subseq=vectSeqG)->p2
           return(p2[order(p2$data$Support,decreasing = TRUE),])
         }
         
       }
     })
     
     discr<-reactive({
       if (req(input$plottypeG) %in% c("Pearson","Pearson.ch")){
         req(subs(),data.select2())
         seqecmpgroup(subs() , group=data.select2()[,"Clustering"])
       }
     })
     
     observe({
       req(discr())
       updateNumericInput(session = session,inputId = "nbAffiche",max=nrow(discr()$data))
     })
     
     # output$subsTableG<-renderUI({
     #   if (req(input$plottypeG) == "Pearson"){
     #     output$tableSubsG<-shiny::renderDataTable({
     #       req(discr())
     #       sousSeqDataG<-cbind(as.character(discr()$subseq),discr()$data)
     #       names(sousSeqDataG)[1]<-("Sous-séquences")
     #       return(sousSeqDataG[(1:6),])
     #     })
     #     return(shiny::dataTableOutput("tableSubsG")%>% withSpinner(color="#0dc5c1"))
     #   }
     # })

     
     #### Choix de sous-séqueneces ####

     ### Mise a jour des inputs permettant de choisir des états ###
     observe({
       input$plottypeG
       isolate({
         if (req(input$plottypeG)=="Pearson.ch"){
           req(seq.select2())
           updateSelectInput(session = session,inputId = "par.sous.seq1G",choices = alphabet(seq.select2()))
           updateSelectInput(session = session,inputId = "par.sous.seq2G",choices = alphabet(seq.select2()))
           updateSelectInput(session = session,inputId = "par.sous.seq3G",choices = cbind("Aucun",alphabet(seq.select2())))
         }
       })
     })
     
     observe({
       updateNumericInput(session = session,inputId = "ligne.supprG",max=nrow(valuesG$df))
     })
     
     valuesG <- reactiveValues()
     valuesG$df <-  as.data.frame(setNames(replicate(3,character(0), simplify = F),c("Etat1","Etat2","Etat3") ))
     
     observeEvent(input$add.buttonG,{
       req(input$par.sous.seq1G,input$par.sous.seq2G)
       newRow <- data.frame(input$par.sous.seq1G, input$par.sous.seq2G,input$par.sous.seq3G)
       colnames(newRow)<-colnames(valuesG$df)
       valuesG$df <- rbind(valuesG$df,newRow)
       rownames(valuesG$df)<-(1:nrow(valuesG$df))
     })
     
     observeEvent(input$delete.buttonG,{
       if(nrow(valuesG$df)>1){
         valuesG$df[!(vect.sous.seq(valuesG$df) %in% as.character(subs()$subseq)[input$ligne.supprG]),]->valuesG$df
         rownames(valuesG$df)<-(1:nrow(valuesG$df))
       }else {
         valuesG$df <- valuesG$df[-nrow(valuesG$df), ]
       }
     })  
     observe({
       req(valuesG$df)
       valuesG$df<-unique(valuesG$df)
     })
     

     
     #### Pour automatiser la taille des graphiques ####

     ordre<-reactive({
       if(input$plottypeG=="flux"){
         input$graph2
         isolate({
           req(grp())     
           return(taille_graph_flux(length(grp())))
         })
       }else ({
         req(grp())     
         return(taille_graph_flux(length(grp())))
       })
     })
     
     haut<-function(){
       ordre2<-ordre()
       return(dim(ordre2)[1]*400)}
     
     
     output$PLOTG <- renderPlot({
       if (req(input$plottypeG)=="flux"){
         req(flux3(),ordre())
         return(marrangeGrob(flux3(), layout_matrix=ordre()))
       }
       if(req(input$plottypeG) %in% c("d", "f", "I", "ms", "mt", "r")){
         # req(data.seq(),input$plottypeG)
         # return(seqplot(seqdata = data.seq(), type = input$plottypeG, group = dataCluster()[,"Clustering"]))
         req(seq.select2(),input$plottypeG)
         return(seqplot(seqdata = seq.select2(), type = input$plottypeG, group = data.select2()[,"Clustering"]))
       }
       if (req(input$plottypeG) =="Pearson"){
         req(discr())
         return(plot(discr()[1:input$nbAffiche]))
       }
       if (req(input$plottypeG) =="Pearson.ch"){
         req(discr())
         return(plot(discr()))
       }
       
     },width = 1300,height = haut)
     
     #### Texte rappelant la sous-population choisie ####
     
     reactive({
       if (req(input$plottypeG)=="flux"){
         input$graph2
         isolate({
           p<-paste("Vous avez sélectionné les groupes",paste(input$var_grp,collapse = ", "))
           if (input$souspop2=="Aucune" || input$souspop2==""){
             return(paste(p,"et auncune sous population"))
           }else{
             
             if (is.factor(data()[,input$souspop2])){
               req(input$souspop_modalite2)
               return(paste(p,"et la sous population",input$souspop2, "avec les modalités",paste(input$souspop_modalite2,collapse = ", ")))
             }
             if (is.numeric(data()[,input$souspop2])){
               
               return(paste(p,"et la sous population",input$souspop2, "entre",min(input$sous_pop_num2,na.rm=TRUE),"et",max(input$sous_pop_num2,na.rm=TRUE)))
               
             }
           }
         })
       }
       
       else {
         if (input$souspop2=="Aucune" || input$souspop2==""){
           return("Vous avez selectionné auncune sous population")
         }else{
           
           if (is.factor(data()[,input$souspop2])){
             req(input$souspop_modalite2)
             return(paste("Vous avez selectionné la sous population",input$souspop2, "avec les modalités",paste(input$souspop_modalite2,collapse = ", ")))
           }
           if (is.numeric(data()[,input$souspop2])){
             
             return(paste("Vous avez selectionné la sous population",input$souspop2, "entre",min(input$sous_pop_num2,na.rm=TRUE),"et",max(input$sous_pop_num2,na.rm=TRUE)))
             
           }
         }
       }
     })->text2
     
     
     renderUI({
       req(text2())
       renderText(text2())->output$textGRP
       h4(textOutput("textGRP"))
     })->output$h4_fluxGrp
     
     
     #### Graphique global pour pouvoir comparer plus facilement ####
     
     fluxGlobal<-eventReactive(eventExpr = input$graph2,{
       req(data.select2(),col_periode2(),seq.select2())
       graph_flux(data=data.select2(),seq_data=seq.select2(),col_periode=col_periode2())
     }
     )
     
     
     output$GraphGlobal<-renderUI({
       output$PlotGlobal<-renderPlot({
         req(input$plottypeG)
         if (req(input$plottypeG) == "flux"){
           req(fluxGlobal())
           return(fluxGlobal())
         }
         if(req(input$plottypeG) %in% c("d", "f", "I", "ms", "mt", "r")){
           req(seq.select2())
           return(seqplot(seqdata = seq.select2(), type = input$plottypeG))
         }
         
       },width=650,height = 400)
       if(req(input$plottypeG) %in% c("d", "f", "I", "ms", "mt", "r","flux")){
         return(plotOutput("PlotGlobal"))
       }
     })
     
     
     output$TitreGlobal<-renderUI({
       if (req(input$plottypeG) %in% c("d", "f", "I", "ms", "mt", "r","flux")){
         return(tags$h4("Graphique Global"))
       }
     })
}