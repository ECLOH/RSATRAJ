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
  data<-reactive({
    req(trajs$dataSource)

    if (input$rowname==TRUE && input$rownames_par=="")
    {
      # userData <- read.csv(file = input$file1$datapath, sep = input$sepcol, encoding = input$endoding)
      # trajs$df<- userData
      colonneID<- c(colnames(trajs$df)[colId(df = trajs$df)])
      updateSelectInput(session = session, inputId = "rownames_par",choices = c("",colonneID))
      }
    if (input$rowname==TRUE && input$rownames_par!=""){
      userData <- read.csv(file = input$file1$datapath, sep = input$sepcol, encoding = input$endoding,row.names = input$rownames_par,header=input$header,na.strings = argna(),dec=input$dec)
      mycolumns<-c(colnames(userData))
      updateSelectInput(session = session, inputId = "timecol", choices = mycolumns)
      trajs$df<- userData

    }

    if(input$rowname==FALSE){
      userData <- read.csv(file = input$file1$datapath, sep = input$sepcol, encoding = input$endoding,header=input$header,na.strings = argna(),dec=input$dec)

      colonneListe <- c(colnames(userData))

      updateSelectInput(session = session, inputId = "timecol", choices = colonneListe)
      trajs$df<- userData}
    return(trajs$df)
  })
  

  output$str<-renderPrint({
    req(data())
    str(data())
  })

  output$contenu<-shiny::renderDataTable({
    req(data())
    data()
  })

  
  

data.seq<-eventReactive(eventExpr = input$ValidParametres, {
  req(data())

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
    seqdef(data()[,input$timecol],cpal = NULL)
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
   # pp<-eventReactive(input$plottype, {
   #   req(data.seq())
   #   plot3<-seqplot(seqdata = data.seq(), type = input$plottype)
   #   plot3
   # })
   # output$PLOT3 <- renderPlot({
   #   req(pp())
   #   pp()
   # },
   # height = 500, width = 1000)
   
   output$PLOT3<-renderPlot({
     req(input$plottype,data.seq())
     seqplot(seqdata = data.seq(), type = input$plottype)
   })
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
     if(input$cluster_type=="CAH"){
       agnes(x = SEQDIST(), method = input$agnes_method)
     } else {
       if(input$cluster_type=="fastCAH"){
         fastcluster::hclust(d = SEQDIST() , method = input$fastclust_method, members = NULL)
#
       }
     }
   })
   output$DENDOGRAM<-renderUI({
     req(SEQCLASS())
     renderPlot(plot(SEQCLASS(), which.plots = 2))->output$dd
     plotOutput("dd") %>% withSpinner(color="#0dc5c1")
   })
        ##### Combinaison de la CAH et de PAM ####

   
#
   ### PLOT G  ####
   ppG<-eventReactive(input$plottype, {
     req(data.seq())
     plotG<-seqplot(seqdata = data.seq(), type = "I", group = sample(x = c("H", "F"), size = 27000, prob = c(0.4, 0.6), replace = TRUE))
     plotG
   })
   output$PLOTG <- renderPlot({
     req(ppG())
     ppG()
   },
   height = 500, width = 1000)
   
   ########################################## Grpahique de flux ##########################################
   
   observeEvent(eventExpr = input$ValidParametres,{
     req(data.seq())
     updateSelectInput(session = session, inputId = "timeseq", choices = input$timecol)
         colsouspop<-colnames(data())[!(colnames(data()) %in% input$timecol)]
         updateSelectInput(session = session, inputId = "souspop", choices = c("Aucune",colsouspop))
   })
   
   observeEvent(input$souspop,{
     req(input$souspop)
     if (input$souspop=="Aucune"){
       updateSelectInput(session = session, inputId = "souspop_modalite", choices = "" )
     }
   })
   
   output$slider<- renderUI({
     if(input$souspop!="Aucune"){
       if (is.numeric(data()[,input$souspop])){
         min<-min(data()[,input$souspop],na.rm = TRUE)
         max<-max(data()[,input$souspop],na.rm = TRUE)
         sliderInput(inputId = "sous_pop_num", label="Slider",min=min,max=max,value = c(min,max))
       }
     }
   })
   output$modalite<- renderUI({
     if(input$souspop!="Aucune"){
       if (is.factor(data()[,input$souspop])){
         selectInput(inputId = "souspop_modalite",label="Modalité", choices = levels(data()[,input$souspop]),selected="",multiple = TRUE)
       }
     }
   })
   
  data.select<-reactive({
    req(input$souspop)
    if (input$souspop=="Aucune" || input$souspop==""){
         data.select1<-data()
        }else{
          req(input$souspop_modalite)
          if (is.factor(data()[,input$souspop])){
            data.select1<-data()[(data()[,input$souspop] %in% c(input$souspop_modalite)),]
          }
          if (is.numeric(data()[,input$souspop]) | is.integer(data()[,input$souspop])){
            
            data.select1<-data()[which(data()[,input$souspop]<= max(input$sous_pop_num,na.rm=TRUE) & data()[,input$souspop]>= min(input$sous_pop_num,na.rm=TRUE)),]
            
          }
         
        }
    return(data.select1)
  })
  output$contenuflux<-shiny::renderDataTable({
    req(data.select())
    data.select()
  })
  
  seq.select<-reactive({
    req(input$souspop)
    if (input$souspop=="Aucune" || input$souspop==""){
      seq.select1<-data.seq()
    }else{
      req(input$souspop_modalite)
      if (is.factor(data()[,input$souspop])){
        seq.select1<-data.seq()[(data()[,input$souspop] %in% c(input$souspop_modalite)),]
      }
      if (is.numeric(data()[,input$souspop]) | is.integer(data()[,input$souspop])){
        
        seq.select1<-data.seq()[which(data()[,input$souspop]<= max(input$sous_pop_num,na.rm=TRUE) & data()[,input$souspop]>= min(input$sous_pop_num,na.rm=TRUE)),]
        
      }
    }
    return(seq.select1)
  })

   col_periode<-eventReactive(eventExpr = length(input$timeseq)>=2,{
     input$timeseq
   })
   ############## Global###################
   flux1<-eventReactive(eventExpr = input$graph1,{
     req(data.seq(),col_periode())
     graph_flux(data=data.select(),seq_data=seq.select(),col_periode=col_periode())
   }
  )
   
   output$flux_global<-renderPlot({
     req(flux1())
     flux1()
   })
   
   
   
   ############### Ensemble ###############
   
   grp<-reactive({
     req(data.select(),input$var_grp)
     unique(data.select()[,input$var_grp])
   })
   
   
   flux3<-eventReactive(eventExpr = input$graph3,{
     req(seq.select(),col_periode(),grp())

     lapply(1:length(grp()), FUN=function(i){
       graph_flux_grp(data = data.select(),seq_data = seq.select(),col_periode(),var_grp = input$var_grp,label_grp = as.character(grp()[i]))

     })
})
      ordre<-reactive({
        req(grp())
        return(taille_graph_flux(length(grp())))
      })
      haut<-function(){
        ordre2<-ordre()
        return(dim(ordre2)[1]*400)}

   output$flux_ens<-renderPlot({
     req(ordre())
     marrangeGrob(flux3(), layout_matrix=ordre())

   }, width = 1300,height= haut)

   eventReactive(eventExpr = input$graph3,{
     if (input$souspop=="Aucune" || input$souspop==""){
       return("Vous avez sélectionné auncune sous population")
     }else{
       req(input$graph3,input$souspop_modalite)
       if (is.factor(data()[,input$souspop])){
         return(paste("Vous avez sélectionné la sous population",input$souspop, "avec les modalités",paste(input$souspop_modalite,collapse = ", ")))
       }
       if (is.numeric(data()[,input$souspop]) | is.integer(data()[,input$souspop])){

         return(paste("Vous avez sélectionné la sous population",input$souspop, "entre",min(input$sous_pop_num,na.rm=TRUE),"et",max(input$sous_pop_num,na.rm=TRUE)))

       }
     }
     
              })->text3
   renderUI({
     req(text3())
     renderText(text3())->output$textEns
     h4(textOutput("textEns"))
     })->output$h2_fluxens
   ################ 1 Groupe ###################
   observeEvent(eventExpr = grp(),{
     updateSelectInput(session = session,inputId = "id_grp",choices = grp(),selected = input$id_grp )
   })
  
   flux2<-eventReactive(eventExpr = input$graph2,{
     req(data.select(),seq.select(),col_periode(),grp(),input$id_grp)
     graph_flux_grp(data = data.select(),seq_data = seq.select(),col_periode(),var_grp = input$var_grp,label_grp = input$id_grp)
     
   }
   )
   
   output$flux_1grp<-renderPlot({
     req(flux2())
     flux2()
   })
   
   }

