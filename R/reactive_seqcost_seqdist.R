trajs.forclass<-reactive({
  if(input$selection_rows=="Sample"){
    sample(x = 1:nrow(trajs), size = 0.2*nrow(trajs), replace = FALSE)->vec.sample
    trajs[vec.sample , ]
  } else {
    trajs
  }
})
SEQCOST<-reactive({
  seqcost(seqdata=trajs.forclass(), 
        method = input$method_edit_cost, 
        cval = input$subst_ratio, 
        time.varying=input$time_varying_substitution_costs,
        transition=input$transition_substitution_costs,
        lag=input$lag_subst_cost)
})
SEQDIST<-reactive({
  seqdist(seqdata = trajs.forclass(), method = input$classtype, refseq = input$refseq_seqdist, norm = input$norm_seqdist, indel = SEQCOST()$indel, sm = SEQCOST()$sm, expcost = input$expcost_seqdist, context=input$context_seqdist)
})
