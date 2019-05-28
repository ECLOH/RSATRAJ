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
        updateSelectInput(session = session, inputId = "timecol", 
                          choices = mycolumns, selected = "PrestationRSA.SituationDossierRSA.EtatDossierRSA.ETATDOSRSA")
        updateSelectInput(session = session, inputId = "IDvar", 
                          choices = mycolumns, selected = "Code")
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
        fluidRow(column(6, shiny::selectInput(inputId = "PICKDATE1", label = "Debut:",
                           choices = names.pick, multiple = FALSE)),
                 column(6, shiny::selectInput(inputId = "PICKDATE1", label = "Fin:",
                             choices = names.pick, multiple = FALSE))
                 )->the.ui
      } else {h3("error")->the.ui}
    }
    the.ui
      })
  
  ###### DATA for traj (à partir de la liste) ####
  #### SELECTION D'UN STOCK DE BENEFICIAIRES
  #   lapply(mycolumns, function(nami){
  #     print(nami)
  #     unique(as.character(unlist(lapply(csv.list, function(listi){
  #       if(!nami%in%names(listi)){ 
  #         } else {
  #           if(class(listi[ , nami])%in%c("numeric", "integer")){
  #             shiny::sliderInput(inputId = "NumSelect", label = "Valeurs sélectionnées", 
  #                                min = min(listi[ , nami], 
  #                                          na.rm = TRUE), max = max(listi[ , nami], 
  #                                                                   na.rm = TRUE), 
  #                                value = c(min(listi[ , nami], 
  #                                              na.rm = TRUE), max(listi[ , nami], 
  #                                                                       na.rm = TRUE))) 
  #           } else {
  #             if(class(listi[ , nami])%in%c("factor", "character")){
  #               if(length(unique(listi[ , nami]))<50){
  #                 shiny::selectInput(inputId = "FactSelect", label="Valeurs sélectionnées", 
  #                                    choices =  unique(listi[ , nami]), multiple = TRUE)
  #               } else {"Trop de modalités"}
  #             }
  #       }
  #     }))))->SELECT_INPUT
  #     #vec<-vec[!is.na(vec)]
  #     #vec<-paste(vec, collapse = " ,\n")
  #     SELECT_INPUT#vec
  #   })->vecmod
  #   names(vecmod)<-mycolumns
  #   vecmod
  # })
  # output$INPUTS_POUR_ADD_ROW<-renderUI({
  #   req(list_names_mod())
  #   fluidRow(column(6, shiny::selectInput(inputId = "ADD_ROW_VAR", label = "Variable:", 
  #                  choices = names(list_names_mod()), selected = "")),
  #            column(6, shiny::selectInput(inputId = "ADD_ROW_MOD", label = "Choix des modalités :", 
  #                      choices = "", multiple = TRUE))
  #   )
  # })
  
  # output$UI_SELECT_DF_TO_SUBSET<-renderUI({
  # names(list_csv())->names.pick
  # shiny::selectInput(inputId = "SELECT_DF_TO_SUBSET", label = "Date pour la sélection : ", 
  #                    choices = names.pick, multiple = FALSE, width = '50%')
  # })
  # df_FOR_SUBSET<-reactive({
  #   list_csv()[[input$SELECT_DF_TO_SUBSET]]
  # })
  # xi <- reactive({
  #   req(df_FOR_SUBSET() )
  #   nbLevelMax <- 15
  #   df_FOR_SUBSET()[,names(which(sapply(df_FOR_SUBSET(), nlevels) < nbLevelMax))]
  #   #df_FOR_SUBSET()
  # })
  # observe({
  #   req(xi())
  #   res <- callModule(module = filterDataServer, id = "ex", x = xi, return_data = FALSE)
  #   
  # })
  # data_filtered <- eventReactive(input$FILTER, {
  #   # Using lazyeval
  #   #filters1 <- lazyeval::lazy_eval(res$expr, data = x())
  #   
  #   # Using base R (my favorite one)
  # filters2 <- eval(expr = res$expr, envir = x())
  #   
  #   # Using rlang
  #   #filters3 <- rlang::eval_tidy(res$expr, data = x())
  #   
  #   #stopifnot(identical(filters1, filters2))
  #   #stopifnot(identical(filters1, filters3))
  #   
  #   # get data filtered
  #  x()[filters2,]
  # })
  
  ###observe({
  ### req(input$ADD_ROW_VAR)
  ###  list_names_mod()[[input$ADD_ROW_VAR]]->select.var.mod
  ###  shiny::updateSelectInput(session = session, inputId = "ADD_ROW_MOD", 
  ###        choices = select.var.mod, selected = "")
  ###  
  ###})
  
  ###### AJOUT CONDITION => ADD ROW #####
  output$TABLE_POUR_SELECTION<-DT::renderDataTable(
  data.frame("DATE"="", "VARIABLE"="", "TYPE"="", "CHOIX"=""),
  server = FALSE,
  rownames = FALSE,
  filter = "top",
  editable = list(target = "row",
                  disable = list(columns = c(1, 2)))
  )
  proxy.TABLE_POUR_SELECTION = dataTableProxy('TABLE_POUR_SELECTION')
  ###### DEFINTION DES VARS ET DES MODALITES ####
  #### SELECT DF DATE ####
  output$UI_DATE_SELECT<-renderUI({
    #reactive({
    names(list_csv())->names.pick
    shiny::selectInput(inputId = "DATE_FOR_SELECT", label = "Date pour sélection:",
                       choices = names.pick, multiple = FALSE)
  })
  #### SELECT VAR et MODLITE ####
  reactive({list_csv()[[input$DATE_FOR_SELECT]]})->the.df
  output$UI_VAR_SELECT<-renderUI({
    req(the.df())
    #mycolumns<-unique(unlist(Reduce(intersect,list(lapply(X = list_csv(), FUN = names))), 
    #                         use.names = FALSE))
    selectInput(inputId = "VAR_FOR_SELECT", label = "Variable pour sélection", 
                choices = names(the.df()), multiple = FALSE)
  })
  output$UI_MOD_SELECT<-renderUI({
    req(the.df())
    the.df()[ , input$VAR_FOR_SELECT]->the.var
    if(class(the.var)%in%c("numeric", "integer")){
      shiny::sliderInput(inputId = "NumSelect", label = "Valeurs sélectionnées", 
                         min = min(the.var, 
                                   na.rm = TRUE), max = max(the.var, 
                                                            na.rm = TRUE), 
                         value = c(min(the.var, 
                                       na.rm = TRUE), max(the.var, 
                                                          na.rm = TRUE)))
    } else {
      if( class(the.var)%in%c("factor", "character") ){
        if(length(unique(the.var))<100){
          shiny::selectInput(inputId = "FactSelect", label="Valeurs sélectionnées", 
                             choices =  unique(the.var), multiple = TRUE)
        } else {"Trop de modalités"}
      }
    }
  })
  #### ADD ROW ###
   observeEvent(input$addROW, {
     
     req(the.df())
     the.df()[ , input$VAR_FOR_SELECT]->the.var
     if(class(the.var)%in%c("factor", "character")){
       input$FactSelect->moda_select
       TYPE="char"
     } else {
       if(class(the.var)%in%c("numeric", "integer")){
         input$NumSelect->moda_select
         TYPE="num"
         
       }
     }
     print(input$NumSelect)
     print(moda_select)

     print(paste(input$DATE_FOR_SELECT, input$VAR_FOR_SELECT, moda_select ))
     
     data.frame("DATE"=input$DATE_FOR_SELECT,
                "VARIABLE"=input$VAR_FOR_SELECT, 
                "TYPE"=TYPE,
                "CHOIX"=paste(moda_select, collapse = "/"))->vecto
     
     addRow(proxy = proxy.TABLE_POUR_SELECTION, data = vecto)
   })
  
  reactive({
    unique(TABLE_POUR_SELECTION$DATE)->dfs
    list_csv()[names(list_csv() )%in%dfs]->list_for_subset
    lapply(1:length(names(list_for_subset)), function(i){
      list_for_subset[names(list_for_subset)==names(list_for_subset)[i]]->list.i
      subset(TABLE_POUR_SELECTION, TABLE_POUR_SELECTION$DATE==names(list_for_subset)[i])->cond.i
      lapply(1:nrow(cond.i), FUN = function(j){
        strsplit(cond.i$CHOIX[j], split = "/")[[1]]->split.choices
        paste("c(", paste(split.choices, collapse='","'))
        as.numeric(split.choices)->num.choices
        if(cond.i$TYPE=="char"){
         paste(cond.i$VARIABLE[j]%in%split.choices
        } else {
          if(cond.i$TYPE=="num"){
            as.numeric(strsplit(cond.i$CHOIX[j], split = "/"))
            cond.i$VARIABLE[j]>=min(num.choices, na.rm=TRUE)&cond.i$VARIABLE[j]<=max(num.choices, na.rm=TRUE)
            
        }
      })
      
    })
  })
   
  UNIQUE_ID<-reactive({
    lapply(X = test.test, function(data.i){
      data.i[ , input$IDvar]
    })->list_ID
    unique(unlist(list_ID, use.names = FALSE))
  })
  
  data_for_traj<-eventReactive(eventExpr = input$ValidParametres, {
 
    Reduce(function(x, y) merge(x[ , ], 
                                y[ , ], 
                                by=input$IDvar, all=TRUE), list_for_traj)

  })
    
  
  data.seq<-eventReactive(eventExpr = input$ValidParametres, {
  req(data())
<<<<<<< HEAD:OLD_VERSION_elie/server.R
    if(DataType=="fichier"){
=======
    if(input$DataType=="fichier"){

>>>>>>> da08f96630b34e961419812022e6eac9842c88c8:inst/APPLICATION/server.R
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
    
  } 
    } else {
<<<<<<< HEAD:OLD_VERSION_elie/server.R
      if(DataType=="objet"){
        s<-seqdef(data()[,input$timecol],cpal = NULL)
        
=======
      if(input$DataType=="objet"){
>>>>>>> da08f96630b34e961419812022e6eac9842c88c8:inst/APPLICATION/server.R
      }
    }
    if (length(alphabet(s))<=12){
      #permet d'avoir les mêmes couleurs que pour les graphiques de flux
      a<-col_flux(data = data(),seq.data = s)
      attr(s, "cpal") <- unname(a[alphabet(s)])
      return(s)
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

   
   ### Pas de temps voulue pour les graphiques de flux avec au minimun deux pas de temps ###
   col_periode1<-eventReactive(eventExpr = length(input$timeseq1)>=2,{
     input$timeseq1
   })
   
   ### Création d'une liste des graphiques de flux pour pouvoir les tracer côte à côte
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
      ### Création des graphiques pour les deux types de sous-séquences ###
   
   sousSeqPlot<-reactive({
     req(seq.select1())
     if (req(input$plottype) == "sous.seq"){
       #Pour la comparaison des sous-populations, on met les graphiques dans une liste#
       if (input$souspop1!="Aucune" && is.factor(data()[,input$souspop1])) {
         req(input$souspop_modalite1)
         lapply(1:length(input$souspop_modalite1), FUN=function(i){
           seq.select1()[data.select1()[,input$souspop1]==input$souspop_modalite1[i],]->seqSouspop
           seqecreate(seqSouspop[order(row.names(seqSouspop)), ], tevent="state", use.labels=FALSE)->seqGlobal
           titre<-paste("Graphique des sous-séquneces \n pour la variable",input$souspop1,"\n avec la modalité",input$souspop_modalite1)
           sousTitre<-paste("Il y a",nrow(seqSouspop),"individus")
           seqefsub(seqGlobal,pmin.support=input$pmin1)->p
           return(graph_sous_sequences(p)+ggtitle(titre,subtitle = sousTitre))
         })
       } else {
           seqecreate(seq.select1()[order(row.names(seq.select1())), ], tevent="state", use.labels=FALSE)->seqGlobal
           return(list(graph_sous_sequences(seqefsub(seqGlobal,pmin.support=input$pmin1))))
         }
     }else{
       ## Cas où l'utilisateur choisi les sous-séquences ##
         if (req(input$plottype) == "sous.seq.ch"){
           req(values$df)
           #condition d'un data.frame values non vide pour exécuter la suite du code afin de ne pas avoir d'erreur quand la data.frame est vide
           if(nrow(values$df)>0){
               if (input$souspop1!="Aucune" && is.factor(data()[,input$souspop1])) {
                 req(input$souspop_modalite1)
                 lapply(1:length(input$souspop_modalite1), FUN=function(i){
                   
                     seq.select1()[data.select1()[,input$souspop1]==input$souspop_modalite1[i],]->seqSouspop
                     seqecreate(seqSouspop[order(row.names(seqSouspop)), ], tevent="state", use.labels=FALSE)->seqGlobal
                     vectSeq<-vect.sous.seq(data = values$df)
                     seqefsub(seqGlobal,str.subseq=vectSeq)->p
                     titre<-paste("Graphique des sous-séquneces \n pour la variable",input$souspop1,"\n avec la modalité",input$souspop_modalite1)
                     sousTitre<-paste("Il y a",nrow(seqSouspop),"individus")
                     return(graph_sous_sequences(p[order(p$data$Support,decreasing = TRUE),])+ggtitle(titre,subtitle = sousTitre))
                   
                 })
               } else {
                 
                   seqecreate(seq.select1()[order(row.names(seq.select1())), ], tevent="state", use.labels=FALSE)->seqGlobal
                   vectSeq<-vect.sous.seq(data = values$df)
                   seqefsub(seqGlobal,str.subseq=vectSeq)->p
                   return(list(graph_sous_sequences(p[order(p$data$Support,decreasing = TRUE),])))
                 
               }
           }
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
   
   ### Mise en action des boutons ajout et suppression ###
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
   ### Ne grader que des sous-séquences uniques ###
   observe({
     req(values$df)
     values$df<-unique(values$df)
   })
   
    ### Utilisé pour pouvoir supprimer des sous-sequences ###   
   subsGlobal<-reactive({
     if (req(input$plottype) == "sous.seq.ch"){
       req(seq.select1(),values$df)
       if(nrow(values$df)>0){
         vectSeq1<-vect.sous.seq(data = values$df)
         seqefsub(seqecreate(seq.select1()[order(row.names(seq.select1())), ], tevent="state", use.labels=FALSE),str.subseq=vectSeq1)->p1
         return(p1[order(p1$data$Support,decreasing = TRUE),])
       }
       
     }
   })
        #### Graphiques ####
    ### Création d'un ordre de disposition des graphiques selon le nombre de graphiques à afficher ###
   ordre1<-reactive({
     if(req(input$plottype)=="flux") {
       input$graph1
       isolate({
         if (input$souspop1!="Aucune" && is.factor(data()[,input$souspop1])) {
           req(input$souspop_modalite1)     
           return(taille_graph_flux(length(input$souspop_modalite1)))
         }else {
           #Cas où on affiche qu'un seul graphique à l'écran
           return(cbind(1,2))
         }
       })
     }
       if(req(input$plottype) %in% c("d", "f", "I", "ms", "mt", "r","sous.seq","sous.seq.ch")) {
         if (input$souspop1!="Aucune" && is.factor(data()[,input$souspop1])) {
           req(input$souspop_modalite1)     
           return(taille_graph_flux(length(input$souspop_modalite1)))
         }else {
           return(cbind(1,2))
         }
       }
   })
   ### Rend automatique la hauteur des graphiques pour qu'ils soient lisisbles ###
   haut1<-function(){
     ordre3<-ordre1()
     if (req(input$plottype) %in% c("sous.seq","sous.seq.ch")) {
       return(dim(ordre3)[1]*1000)
     }else{
       return(dim(ordre3)[1]*400)
     }
    }
  
   ### Graphiques des statistiques descriptives/ visualisation des trajectoires ###
   output$PLOT3<- renderUI({
     if (req(input$plottype) %in% c("sous.seq","sous.seq.ch")){
       output$PLOT<-renderPlot({
         req(sousSeqPlot(),ordre1())
         return(marrangeGrob(sousSeqPlot(), layout_matrix=ordre1()))
       },width = 1300,height = haut1)
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
     input$calculCLUST
     isolate({
      req(SEQCLASS())
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

   
          #### Tableau indicateurs pour évaluer la qualité des classifiactions testées ####
   
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
   

   ### Création d'une variable "Clustering" donnant la classification choisie ###
   output$classif_grp<-renderUI({
     req(SEQCLASS())
     input$calculCLUST
     input$SliderGrp
     isolate({
       if (input$cluster_type=="CAHPAM"){
         req(input$SliderGrp)
         return(tagList(
           column(4,
                  shiny::numericInput(inputId = "nb_cluster",label="Nombre de groupes choisi",step=1,value = min(input$SliderGrp,na.rm=TRUE)+1,min=min(input$SliderGrp,na.rm=TRUE),max=max(input$SliderGrp,na.rm=TRUE))
           ),
           column(2,
                  shiny::actionButton(inputId = "Bouton_Clustering",label = "Faire les groupes")
           )
         ))
         
       }
       if(input$cluster_type=="CAH"){
         return(tagList(
           column(4,
                  shiny::numericInput(inputId = "nb_cluster",label="Nombre de groupes choisi",step=1,value = 2,min=2 ,max=10)
           ),
           column(2,
                  shiny::actionButton(inputId = "Bouton_Clustering",label = "Faire les groupes")
           )
         ))
       }
     })
   })
   
   
   dataCluster<-eventReactive(eventExpr = input$Bouton_Clustering,{
     req(SEQCLASS())
     if (input$cluster_type=="CAHPAM"){
       indicateur<-Creation_indicateur(nb_cluster_min=min(input$SliderGrp,na.rm=TRUE),nb_cluster_max=max(input$SliderGrp,na.rm=TRUE),mat_dist=SEQDIST(),intialclust=SEQCLASS())
       return(data_cluster(indicateur,data(),input$nb_cluster))
     }
     if(input$cluster_type=="CAH"){
       data()->dataCopieCAH
       clusterCAH<-as.factor(cutree(SEQCLASS(),k = input$nb_cluster))
       dataCopieCAH[,"Clustering"]<-factor(clusterCAH,labels = paste0("G",1:input$nb_cluster))
       
       return(dataCopieCAH)
     }
   })
   
   output$textCluster<-renderText({
     input$Bouton_Clustering
     isolate({
       req(dataCluster())
       return(paste("Vous avez créé",length(levels(dataCluster()[,"Clustering"])),"groupes"))
     })
     
   })
   
#
   ### PLOT G  ####
        ##### Mise a jour des inputs #####
   observeEvent(eventExpr = input$Bouton_Clustering,{
     #pas de temps pour les graphiques de flux
     updateSelectInput(session = session, inputId = "timeseq2", choices = input$timecol)
     #sous population
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

   
      #### Selection de la sous population ####
   data.select2<-reactive({
     req(input$souspop2,dataCluster())
       if (input$souspop2=="Aucune" || input$souspop2==""){
         data.selectG<-dataCluster()
       }else{
         
         if (is.factor(dataCluster()[,input$souspop2])){
           req(input$souspop_modalite2)
           data.selectG<-dataCluster()[which(dataCluster()[,input$souspop2] %in% c(input$souspop_modalite2)),]
         }
         if (is.numeric(dataCluster()[,input$souspop2])){
           req(input$sous_pop_num2)
           data.selectG<-dataCluster()[which(dataCluster()[,input$souspop2]<= max(input$sous_pop_num2,na.rm=TRUE) & dataCluster()[,input$souspop2]>= min(input$sous_pop_num2,na.rm=TRUE)),]
           
         }
         
       }
       return(data.selectG)
   })
   
   seq.select2<-reactive({
     req(input$souspop2)
     
     if (input$souspop2=="Aucune" || input$souspop2==""){
       seq.selectG<-data.seq()
     }else{
       
       if (is.factor(data()[,input$souspop2])){
         req(input$souspop_modalite2)
         seq.selectG<-data.seq()[which(data()[,input$souspop2] %in% c(input$souspop_modalite2)),]
       }
       if (is.numeric(data()[,input$souspop2])){
         req(input$sous_pop_num2)
         seq.selectG<-data.seq()[which(data()[,input$souspop2]<= max(input$sous_pop_num2,na.rm=TRUE) & data()[,input$souspop2]>= min(input$sous_pop_num2,na.rm=TRUE)),]
         
       }
     }
     
     return(seq.selectG)
     
   })
   
   
   ### Proposition des groupes à représenter ### 
   
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
   
   ### Selectionne de la période temporelle ###
   col_periode2<-eventReactive(eventExpr = length(input$timeseq2)>=2,{
     input$timeseq2
   })

   ### Graphique de flux pour les groupes ###
   observeEvent(input$graph2,{
       if(input$plottypeG=="flux"){
         if (input$souspop2!="Aucune" && is.factor(data()[,input$souspop2])) {
           req(ordre())
           tailleGraph$height<-dim(ordre())[1]*400
           lapply(1:length(input$souspop_modalite2), FUN=function(j){
             paste0('SEQPLOTFLUX', j)->id.output
             output[[id.output]] <- renderPlot({
              input$graph2
               isolate({
                 req(data.select2(),seq.select2(),col_periode2(),input$var_grp,input$souspop2,input$souspop_modalite2)
                 return(marrangeGrob(lapply(1:length(input$var_grp), FUN=function(i){
                                  dat<-data.select2()[data.select2()[,"Clustering"]==input$var_grp[i],]
                                  titre<-paste("Graphique de flux des",nrow(dat[dat[,input$souspop2]==input$souspop_modalite2[j],]),"individus du groupe",input$var_grp[i],"\n ayant pour la variable",input$souspop2,"la modalité",input$souspop_modalite2[j])
                                  graph_flux_grp(data = data.select2()[data.select2()[,input$souspop2]==input$souspop_modalite2[j],],seq_data = seq.select2()[data.select2()[,input$souspop2]==input$souspop_modalite2[j],],col_periode2(),var_grp = "Clustering",label_grp = as.character(input$var_grp[i]))+ggtitle(titre)
                                }),layout_matrix = ordre()))
                              })
             },width = 1300,height = haut)
           })
         }
         if(input$souspop2=="Aucune"){
           output$SEQPLOTFLUX <- renderPlot({
            input$graph2
             isolate({
                    req(data.select2(),seq.select2(),col_periode2(),input$var_grp,input$souspop2)
               return(marrangeGrob(lapply(1:length(input$var_grp), FUN=function(i){
                      graph_flux_grp(data = data.select2(),seq_data = seq.select2(),col_periode2(),var_grp = "Clustering",label_grp = as.character(input$var_grp[i]))
                    }),layout_matrix = ordre()))
                  })
             },width = 1300,height = haut)
         }
         if(input$souspop2!="Aucune" && is.numeric(data()[,input$souspop2])){
           output$SEQPLOTFLUX <- renderPlot({
             input$graph2
             isolate({
               req(data.select2(),seq.select2(),col_periode2(),input$var_grp,input$souspop2)
               return(marrangeGrob(lapply(1:length(input$var_grp), FUN=function(i){
                 titre<-paste("Graphique de flux des",nrow(data.select2()[data.select2()[,"Clustering"]==input$var_grp[i],]),"individus du groupe",input$var_grp[i],"\n ayant pour la variable",input$souspop2,"une valeur entre",min(input$sous_pop_num2,na.rm = TRUE),"et",max(input$sous_pop_num2,na.rm = TRUE))
                 graph_flux_grp(data = data.select2(),seq_data = seq.select2(),col_periode2(),var_grp = "Clustering",label_grp = as.character(input$var_grp[i]))+ggtitle(titre)
               }),layout_matrix = ordre()))
             })
           },width = 1300,height = haut)
         }
       }
   })
     ###### Graphique coefficient de Pearson ######
     # 
     # event.seq<-reactive({
     #   if (req(input$plottypeG) %in% c("Pearson","Pearson.ch")){
     #      req(seq.select2(),dataCluster())
     #      return(seqecreate(seq.select2()[order(row.names(seq.select2())), ], tevent="state", use.labels=FALSE))
     #   }
     # })
     # 
     subs<-reactive({
       if (req(input$plottypeG)=="Pearson"){
          req(seq.select2(),dataCluster(),valuesG$df)
          return(seqefsub(seqecreate(seq.select2()[order(row.names(seq.select2())), ], tevent="state", use.labels=FALSE),pmin.support=input$pmin))
       }
       if (req(input$plottypeG) == "Pearson.ch"){
         req(seq.select2(),dataCluster(),valuesG$df)
         if(nrow(valuesG$df)>0){
           unique(c(levels(valuesG$df[,1]),levels(valuesG$df[,2]),levels(valuesG$df[,3])))->valCh
           valCh[valCh!="Aucun"]->valCh
           seqecreate(seq.select2()[order(row.names(seq.select2())), ], tevent="state", use.labels=FALSE)->seqGlobal22
           
           if(all(valCh %in% alphabet(seqGlobal22))){
             vectSeqG<-vect.sous.seq(data = valuesG$df)
             seqefsub(seqGlobal22,str.subseq=vectSeqG)->p22
             return(p22[order(p22$data$Support,decreasing = TRUE),])
           }else{
             valCh[!(valCh %in% alphabet(seqGlobal22))]->valnonalphabet
             valuesG$df<-valuesG$df[which(!(valuesG$df[,1] %in% valnonalphabet | valuesG$df[,2] %in% valnonalphabet | valuesG$df[,3] %in% valnonalphabet)),]
             if(nrow(valuesG$df)>0){
               vectSeqG<-vect.sous.seq(data = valuesG$df)
               seqefsub(seqGlobal22,str.subseq=vectSeqG)->p22
               return(p22[order(p22$data$Support,decreasing = TRUE),])
             }
           }
         }

       }
     })
     
     discr<-reactive({
       if (req(input$plottypeG) %in% c("Pearson","Pearson.ch")){
         req(subs(),data.select2())
         if(nrow(valuesG$df)>0){
         seqecmpgroup(subs() , group=data.select2()[,"Clustering"])
         } 
       }
     })

     output$alpabeltTexte<-renderUI({
       output$TexteAlpha<-renderText({
         if (req(input$plottypeG) == "Pearson.ch"){
           req(seq.select2(),dataCluster(),valuesG$df)
           seqecreate(seq.select2()[order(row.names(seq.select2())), ], tevent="state", use.labels=FALSE)->seqGlobalText
           return(paste("Selectionnez des états se trouvant dans la liste suivante :",paste(alphabet(seqGlobalText),collapse = ", ")))
         }
       })
       return(textOutput("TexteAlpha"))
     })
     
     
  observe({
     req(seq.select2(),dataCluster(),ordre())
     if (req(input$plottypeG) == "Pearson"){
       #Pour la comparaison des sous-populations, on met les graphiques dans une liste#
       if (input$souspop2!="Aucune" && is.factor(data()[,input$souspop2])) {
         req(input$souspop_modalite2)
         tailleGraph$height<-dim(ordre())[1]*400
         lapply(1:length(input$souspop_modalite2), FUN=function(i){
           paste0('SEQPLOTPEARSON', i)->id.output
           output[[id.output]] <- renderPlot({
             if (req(input$plottypeG) == "Pearson"){
               if (input$souspop2!="Aucune" && is.factor(data()[,input$souspop2])) {
                 seq.select2()[data.select2()[,input$souspop2]==input$souspop_modalite2[i],]->seqSouspop2
                 seqecreate(seqSouspop2[order(row.names(seqSouspop2)), ], tevent="state", use.labels=FALSE)->seqGlobal2
                 # titre<-paste("Graphique des sous-séquneces \n pour la variable",input$souspop2,"\n avec la modalité",input$souspop_modalite1)
                 # sousTitre<-paste("Il y a",nrow(seqSouspop),"individus")
                 seqefsub(seqGlobal2,pmin.support=input$pmin)->p2
                 return(plot(seqecmpgroup(p2 , group=data.select2()[data.select2()[,input$souspop2]==input$souspop_modalite2[i],"Clustering"])[1:input$nbAffiche]))
               }
             } 
            },height = haut,width = 1300)
         })
       } else {
         output$SEQPLOTPEARSON<-renderPlot({
           if (req(input$plottypeG) == "Pearson"){
             if (input$souspop2=="Aucune" || is.numeric(data()[,input$souspop2])) {
               seqecreate(seq.select2()[order(row.names(seq.select2())), ], tevent="state", use.labels=FALSE)->seqGlobal2
               seqefsub(seqGlobal2,pmin.support=input$pmin)->p2
               return(plot(seqecmpgroup(p2 , group=data.select2()[,"Clustering"])[1:input$nbAffiche]))
             }
           }  
         },height = haut,width = 1300)
         
       }
     }else{
       ## Cas où l'utilisateur choisi les sous-séquences ##
       if (req(input$plottypeG) == "Pearson.ch"){
         req(valuesG$df)
         #condition d'un data.frame values non vide pour exécuter la suite du code afin de ne pas avoir d'erreur quand la data.frame est vide
         if(nrow(valuesG$df)>0){
           unique(c(levels(valuesG$df[,1]),levels(valuesG$df[,2]),levels(valuesG$df[,3])))->valCh
           valCh[valCh!="Aucun"]->valCh
           if (input$souspop2!="Aucune" && is.factor(data()[,input$souspop2])) {
             req(input$souspop_modalite2)
             tailleGraph$height<-dim(ordre())[1]*400
             lapply(1:length(input$souspop_modalite2), FUN=function(i){
               paste0('SEQPLOTPEARSONCH', i)->id.output
               output[[id.output]] <- renderPlot({
                 if (req(input$plottypeG) == "Pearson.ch"){
                   if (input$souspop2!="Aucune" && is.factor(data()[,input$souspop2])) {
                     seq.select2()[data.select2()[,input$souspop2]==input$souspop_modalite2[i],]->seqSouspop2
                     seqecreate(seqSouspop2[order(row.names(seqSouspop2)), ], tevent="state", use.labels=FALSE)->seqGlobal2
                     if(all(valCh %in% alphabet(seqGlobal2))){
                       if(nrow(valuesG$df)>0){
                         vectSeq2<-vect.sous.seq(data = valuesG$df)
                         seqefsub(seqGlobal2,str.subseq=vectSeq2)->p2
                         return(plot(seqecmpgroup(p2[order(p2$data$Support,decreasing = TRUE),] , group=data.select2()[data.select2()[,input$souspop2]==input$souspop_modalite2[i],"Clustering"])))
                        }
                       
                     }else{
                       valCh[!(valCh %in% alphabet(seqGlobal2))]->valnonalphabet
                       valuesG$df<-valuesG$df[which(!(valuesG$df[,1] %in% valnonalphabet | valuesG$df[,2] %in% valnonalphabet | valuesG$df[,3] %in% valnonalphabet)),]
                       if(nrow(valuesG$df)>0){
                         vectSeq2<-vect.sous.seq(data = valuesG$df)
                         seqefsub(seqGlobal2,str.subseq=vectSeq2)->p2
                         return(plot(seqecmpgroup(p2[order(p2$data$Support,decreasing = TRUE),] , group=data.select2()[data.select2()[,input$souspop2]==input$souspop_modalite2[i],"Clustering"])))
                       }
                     }
                   }
                 } 
                },height = haut,width = 1300)
             })
           } else {
             output$SEQPLOTPEARSONCH<-renderPlot({
               if (req(input$plottypeG) == "Pearson.ch"){
                 if (input$souspop2=="Aucune" || is.numeric(data()[,input$souspop2])) {
                   if(nrow(valuesG$df)>0){
                     seqecreate(seq.select2()[order(row.names(seq.select2())), ], tevent="state", use.labels=FALSE)->seqGlobal2
                     vectSeq2<-vect.sous.seq(data = valuesG$df)
                     seqefsub(seqGlobal2,str.subseq=vectSeq2)->p2
                     return(plot(seqecmpgroup(p2[order(p2$data$Support,decreasing = TRUE),] , group=data.select2()[,"Clustering"])))
                   }
                 }   
               }
            },height = haut,width = 1300)
           }
         }
       }
     }
   })
   
   
   
     observe({
       if(nrow(valuesG$df)>0){
          req(discr())
          updateNumericInput(session = session,inputId = "nbAffiche",max=nrow(discr()$data))
       }else{
         updateNumericInput(session = session,inputId = "nbAffiche",max=1)
       }
     })
     
     
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
           req(input$var_grp)     
           return(taille_graph_flux(length(input$var_grp)))
         })
       }else ({
         req(grp())     
         return(taille_graph_flux(length(grp())))
       })
     })
     
     haut<-function(){
       ordre2<-ordre()
       return(dim(ordre2)[1]*400)}
     
     tailleGraph<-reactiveValues(height=800)
     
     output$PLOTG <- renderUI({
       
         if (req(input$plottypeG)=="flux"){
           input$graph2
           isolate({
             if (input$souspop2!="Aucune" && is.factor(data()[,input$souspop2])) {
               req(input$souspop_modalite2,col_periode2(),input$var_grp)
               return(tagList(lapply(1:length(input$souspop_modalite2), function(i) {
                 plotOutput(paste0('SEQPLOTFLUX', i),height = tailleGraph$height)
               })))
             }
             else{
               req(col_periode2(),input$var_grp)
               return(plotOutput("SEQPLOTFLUX"))
             }

           })
           #output$plotGrp<-renderPlot({
           #   req(flux3(),ordre())
           #    return(marrangeGrob(flux3(), layout_matrix=ordre()))
           # },width = 1300,height = haut)
           # return(plotOutput("plotGrp")%>% withSpinner(color="#0dc5c1"))
         }
      
       
       # if (req(input$plottypeG) =="Pearson"){
       #   
       #   output$plotGrp<-renderPlot({
       #     req(sousSeqPlotG(),ordre())
       #     return(plot(sousSeqPlotG()[1:input$nbAffiche]))
       #   },width = 1300,height = haut)
       #   return(plotOutput("plotGrp"))
       # }
       if (req(input$plottypeG) == "Pearson"){
         if (input$souspop2!="Aucune" && is.factor(data()[,input$souspop2])) {
             return(tagList(lapply(1:length(input$souspop_modalite2), function(i) {
               plotOutput(paste0('SEQPLOTPEARSON', i),height = tailleGraph$height)
             })))
         }else{
           return(plotOutput("SEQPLOTPEARSON"))
         }
       }
       if (req(input$plottypeG) == "Pearson.ch"){
         if (input$souspop2!="Aucune" && is.factor(data()[,input$souspop2])) {
           return(tagList(lapply(1:length(input$souspop_modalite2), function(i) {
             plotOutput(paste0('SEQPLOTPEARSONCH', i),height = tailleGraph$height)
           })))
         }else{
           return(plotOutput("SEQPLOTPEARSONCH"))
         }
       }
       if(req(input$plottypeG) %in% c("d", "f", "I", "ms", "mt", "r")){
         req(input$souspop2)
         if (input$souspop2=="Aucune"){
           return(plotOutput("plotGrp"))
         }else{
          if (is.factor(data()[,input$souspop2])) {
            return(tagList(lapply(1:length(input$souspop_modalite2), function(i) {
              plotOutput(paste0('SEQPLOT', i),height = tailleGraph$height)
            })))
          }
          if(is.numeric(data()[,input$souspop2])){
             return(plotOutput("plotGrp"))
          }
         }

         
       }
     })
     ### Création des renderplots pour les graphiques seqplot ###
     observe({
       req(seq.select2())
       if(req(input$plottypeG) %in% c("d", "f", "I", "ms", "mt", "r")){
         if(input$souspop2=="Aucune"){
           output$plotGrp <- renderPlot({
             req(seq.select2(),ordre())
             if(req(input$plottypeG) %in% c("d", "f", "I", "ms", "mt", "r")){
               if(input$souspop2=="Aucune"){
                seqplot(seqdata = seq.select2(), type = input$plottypeG, group = data.select2()[,"Clustering"])
               }
             } 
           },height = haut,width = 1300)
         }else{
           if (is.factor(data()[,input$souspop2])) {
             req(input$souspop_modalite2)
             
             lapply(1:length(input$souspop_modalite2), FUN=function(i){
               paste0('SEQPLOT', i)->id.output
               output[[id.output]] <- renderPlot({
                 req(seq.select2(),ordre())
                 if(req(input$plottypeG) %in% c("d", "f", "I", "ms", "mt", "r")){
                   if(input$souspop2!="Aucune" && is.factor(data()[,input$souspop2])){
                     tailleGraph$height<-dim(ordre())[1]*400
                     seqplot(seqdata = seq.select2()[which(data.select2()[,input$souspop2]==input$souspop_modalite2[i]),], type = input$plottypeG, group = data.select2()[which(data.select2()[,input$souspop2]==input$souspop_modalite2[i]),"Clustering"],main = paste("Graphique de la variable",input$souspop2,"avec la modalité",input$souspop_modalite2[i]))
                   }
                 } 
                 },height = haut,width = 1300)
               
             })
           }
           if(is.numeric(data()[,input$souspop2])){
             output$plotGrp<-renderPlot({
               req(seq.select2(),ordre(),input$sous_pop_num2)
               if (req(input$plottypeG) %in% c("d", "f", "I", "ms", "mt", "r")){
                 if(input$souspop2!="Aucune" && is.numeric(data()[,input$souspop2])){
                   titre<-paste("Graphique de la variable",input$souspop2,"entre",min(input$sous_pop_num2,na.rm = TRUE),"et",max(input$sous_pop_num2,na.rm = TRUE))
                   return(seqplot(seqdata = seq.select2(), type = input$plottypeG, group = data.select2()[,"Clustering"],main = titre))
                 }
               } 
               },height = haut,width = 1300)
           }
         }
       }
     })
     
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
     
     #### Statistiques descriptives Groupes ####

     ##### Mise a jour des inputs #####
     observeEvent(eventExpr = input$Bouton_Clustering,{
       #sous population
       colsouspop2<-colnames(data())[!(colnames(data()) %in% input$timecol)]
       updateSelectInput(session = session, inputId = "souspop2StatDesc", choices = c("Aucune",colsouspop2))
       
     })
     
     # observe({
     #   req(grp())
     #   updateSelectInput(session = session, inputId = "GrpStatDesc", choices = grp())
     # })
     
     # observeEvent(input$souspop2,{
     #   req(input$souspop2StatDesc)
     #   if (input$souspop2StatDesc=="Aucune"){
     #     updateSelectInput(session = session, inputId = "souspop_modalite2StatDesc", choices = "" )
     #   }
     # })
     # 
     # output$slider2StatDesc<- renderUI({
     #   if(input$souspop2StatDesc!="Aucune"){
     #     if (is.numeric(data()[,input$souspop2StatDesc])){
     #       min<-min(data()[,input$souspop2StatDesc],na.rm = TRUE)
     #       max<-max(data()[,input$souspop2StatDesc],na.rm = TRUE)
     #       sliderInput(inputId = "sous_pop_num2StatDesc", label="Slider",min=min,max=max,value = c(min,max))
     #     }
     #   }
     # })
     # output$modalite2StatDesc<- renderUI({
     #   if(input$souspop2StatDesc!="Aucune"){
     #     if (is.factor(data()[,input$souspop2StatDesc])){
     #       selectInput(inputId = "souspop_modalite2StatDesc",label="Modalité", choices = levels(data()[,input$souspop2StatDesc]),selected="",multiple = TRUE)
     #     }
     #   }
     # })
     
     ### Tableau des profils lignes ###
     
     output$outils<-renderUI({
         if (input$souspop2StatDesc=="Aucune"){
           shiny::radioButtons(inputId = "TypeGraph", label = "Graphique",choices=c("Effectif","Pourcentage"))
         }
     })
     
     # Tableau des effectifs et proportions dans chaque groupe
     tableEffectifR<-reactive({
       if (input$souspop2StatDesc=="Aucune"){
         cbind(summary(dataCluster()[,"Clustering"]),round((summary(dataCluster()[,"Clustering"])/nrow(dataCluster()))*100,2))->tableEffectif
         colnames(tableEffectif)<-c("Effectif","Proportion")
         apply(tableEffectif,2,sum)->totGlobal #Rajout de la ligne Global
         rbind(tableEffectif,totGlobal)->tableEffectif
         rownames(tableEffectif)[dim(tableEffectif)[1]]<-"Global"
         as.integer(tableEffectif[,1])->tableEffectif[,1] # Pour enlever les décimales dans la colonne effectif
         return(tableEffectif)
       }
     })
     
     output$profilLigne<-renderUI({
        req(dataCluster())
       if (input$souspop2StatDesc!="Aucune" && is.factor(data()[,input$souspop2StatDesc])){
         output$tableauProfilLigne <- renderDataTable({
           if (input$souspop2StatDesc!="Aucune" && is.factor(data()[,input$souspop2StatDesc])){
            tableau_ligne(data = dataCluster(),var_grp = "Clustering",var = input$souspop2StatDesc)
           }
          })
         output$graphStatDescGrpF<-renderPlot({
           if (input$souspop2StatDesc!="Aucune" && is.factor(data()[,input$souspop2StatDesc])){
             table(dataCluster()[,"Clustering"],dataCluster()[,input$souspop2StatDesc])->dataplot
             return(barplot(dataplot,beside=T,legend=rownames(dataplot),main="Effectif par groupe et par modalité"))
           }
         })
         return(tagList(tags$h4("Profil ligne"),dataTableOutput("tableauProfilLigne"),plotOutput("graphStatDescGrpF")))
       }
       if (input$souspop2StatDesc=="Aucune"){
         output$tableEff<-renderDataTable({
           if (input$souspop2StatDesc=="Aucune"){
             req(tableEffectifR())
             tableEffectifR()
           }
         },rownames = TRUE)
         output$GraphEffectiGrp<-renderPlot({
           if (input$souspop2StatDesc=="Aucune"){
             req(tableEffectifR())
             if (input$TypeGraph=="Effectif"){
               return(barplot(tableEffectifR()[1:(dim(tableEffectifR())[1]-1),1],main = "Répartition des effectifs par groupe"))
             }
             if (input$TypeGraph=="Pourcentage"){
               return(barplot(tableEffectifR()[1:(dim(tableEffectifR())[1]-1),2],main = "Répartition en pourcentage des effectifs par groupe"))
             }
           }
         })
         return(tagList(tags$h4("Effectif pour chaque groupe"),dataTableOutput("tableEff"),plotOutput("GraphEffectiGrp")))
       }
     })
     
     
     # #### Graphique global pour pouvoir comparer plus facilement ####
     # 
     # fluxGlobal<-eventReactive(eventExpr = input$graph2,{
     #   req(data.select2(),col_periode2(),seq.select2())
     #   graph_flux(data=data.select2(),seq_data=seq.select2(),col_periode=col_periode2())
     # }
     # )
     # 
     # 
     # output$GraphGlobal<-renderUI({
     #   output$PlotGlobal<-renderPlot({
     #     req(input$plottypeG)
     #     if (req(input$plottypeG) == "flux"){
     #       req(fluxGlobal())
     #       return(fluxGlobal())
     #     }
     #     if(req(input$plottypeG) %in% c("d", "f", "I", "ms", "mt", "r")){
     #       req(seq.select2())
     #       return(seqplot(seqdata = seq.select2(), type = input$plottypeG))
     #     }
     #     
     #   },width=650,height = 400)
     #   if(req(input$plottypeG) %in% c("d", "f", "I", "ms", "mt", "r","flux")){
     #     return(plotOutput("PlotGlobal"))
     #   }
     # })
     # 
     # 
     # output$TitreGlobal<-renderUI({
     #   if (req(input$plottypeG) %in% c("d", "f", "I", "ms", "mt", "r","flux")){
     #     return(tags$h4("Graphique Global"))
     #   }
     # })
}