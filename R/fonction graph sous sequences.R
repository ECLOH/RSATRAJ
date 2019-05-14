#' Fonction graph_sous_sequences
#'
#' @param subs sortie de la fonction seqefsub du package TraMineR
#'
#' @return Un graphique
#' @export
graph_sous_sequences<-function(subs){
  cbind(as.character(subs$subseq),subs$data)->datasubs
  ("soussequence")->names(datasubs)[1]
  datasubs[,1]<-as.character(datasubs[,1])
  #str_replace_all(datasubs[,1], fixed(")-("), " \n ")->datasubs[,1]
  
  #gg<-ggplot(data=datasubs,aes(x=reorder(soussequence,-Support),y=Support)) +geom_bar(stat='identity',width=0.9,fill="grey74")+geom_text(aes(label=round(Support,2)),vjust=-1)+ theme_classic()+theme(axis.text.x = element_text(size=9, angle=30, vjust=0.8, hjust=0.6),axis.title.x = element_blank())+ylim(0,1)+ggtitle("Support des sous-séquences")
  
  
  gg<-ggplot(data=datasubs,aes(x=reorder(soussequence,-Support),y=Support)) +geom_bar(stat='identity',width=0.9,fill="grey74")+geom_text(aes(label=round(Support,2)),vjust=-1)+ theme_classic()+theme(axis.text.x = element_text(size=9, angle=90, hjust=1,vjust = 0.3),axis.title.x = element_blank(),plot.title = element_text(hjust = 0.5,size=18,face = "bold"),plot.subtitle = element_text(hjust = 0.5,size=14))+ylim(0,1)+ggtitle("Support des sous-séquences")
  return(gg)
}

#' @examples


