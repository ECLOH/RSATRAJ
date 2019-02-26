#### ARGS by methods ####
  ##### seqcost ######
  cost.args<-list(
  "CONSTANT"=c("cval"),
  "TRATE"=c("cval", "time.varying", "transition", "lag"),
  "FUTURE"=c(),
  "INDELS"=c(),
  "INDELSLOG"=c()
  )
  ##### seqdist ######
  seqdist.args<-list(
    "edit"=list(
    "OM"=c("norm", "refseq"),
  "OMloc"=c("expcost", "context", "refseq"),
  #"OMslen"=c("link", "h", "refseq"),
  #"OMspell"=c("tpow", "expcost", "refseq"),
  #"OMstran"=c("transindel", "otto", "previous", "add.column", "weighted"),
  "HAM"=c("norm", "refseq"),
  "DHD"=c("norm", "refseq")
  #"TWED"=c("h", "nu", "refseq")),
    ),
  "common_attributes"=list(
  "LCS"=c("norm", "refseq"), 
  "LCP"=c("norm", "refseq"), 
  "RLCP"=c("norm", "refseq")
  ),
  #"NMS"=c("prox", "kweights", "refseq"),
  #"NMSMST"=c("kweights", "tpow", "refseq"),
  #"SVRspell"=c("prox", "kweights", "tpow", "refseq")),
  "distrib"=list(
  #"CHI2"=c("breaks", "step", "overlap", "weighted", "norm"),
  #"EUCLID"=c("breaks", "step", "overlap", "weighted", "norm"))
  )
  )
#### INPUTS for args ####
  ##### seqcost ######
  edit.cost.inputs<-list(
    "cval"=sliderInput(label = "Coûts de substitution: rapport aux coûts indel (1)", inputId = "subst_ratio", min = 0.1, max = 5, step = 0.1, value = 2, width = "80%"),
    "time.varying"=checkboxInput(inputId="time_varying_substitution_costs", 
                                 label="Les taux de transitions sont-ils dépendants du temps?", 
                                 value = FALSE, width = NULL),
    "transition"=selectInput(inputId = "transition_substitution_costs", label = "Type de transition", choices = c("previous" , "next", "both"), selected = "both", multiple = FALSE),
    "lag"=shiny::numericInput(inputId = "lag_subst_cost", label ="Pas de temps pour le calcul des taux de transition", value = 1, min = 1, max = 36, step = 1)
  )
    ##### seqdist ######
  seqdist.inputs<-list(
    "norm"=selectInput(inputId = "norm_seqdist", label = "seqdist(norm = )", choices = c("none" , "auto","maxlength", "gmean", "maxdist", "YujianBo"), selected = "none", multiple = FALSE),
    "refseq"=list(
      shiny::checkboxInput(inputId = "refseq_LOG_seqdist", label = "Calculer les distance par rapport à une trajectoire de référence?",value = FALSE),
      conditionalPanel(condition = "input.refseq_LOG_seqdist == 1", 
                       shiny::numericInput(inputId = "refseq_seqdist", label ="Trajectoire de référence", value=0, min = 0, step = 1))),
    "expcost"=shiny::numericInput(inputId = "expcost_seqdist", label ="Coût de modification de la taille des séuences", value = 0.5, min = 0, step = 0.1),
    "context"=shiny::numericInput(inputId = "context_seqdist", label ="Coût d'insertion (default: 1-2*expcost)", value = 1-2*0.5, min = 0, step = 0.1)
  )
save(cost.args, seqdist.args,edit.cost.inputs,seqdist.inputs, file="data/method_args.RData")
