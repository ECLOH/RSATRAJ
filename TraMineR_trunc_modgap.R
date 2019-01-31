TraMineR.trunc.modgap <- function(seqdata, mstate, sl, left = "DEL", right = "DEL",
                           gaps = "DEL", neutral = "#", void = "%", minimal.gap, regle.pour.faux.gap) {
  
  sidx <- 1:sl
  
  ## Index des missing et index des etats valides
  na.pos <- sidx[mstate]
  notna.pos <- sidx[!mstate]
  
  ## Position du premier etat valide
  c1 <- notna.pos[1]
  
  if (c1>1) {
    lc <- c1-1
  } else {lc=0}
  
  rc <- max(notna.pos)+1
  mm <- na.pos[na.pos > lc+1 & na.pos < rc-1]
  mm.num<-as.numeric(mm)
  
  df.gap<-data.frame("mm"=mm, 
             "grouped.gap"=sapply(X = 1:length(mm.num), FUN = function(i){
               if(length(mm.num)<=1) FALSE else {
    if(i==length(mm.num)){
      if(mm.num[i-1]==mm.num[i]-1){
        TRUE
      } else FALSE } else {
        if(i==1){
          if(mm.num[i+1]==mm.num[i]+1){
            TRUE
          } else FALSE 
        } else {
          if(mm.num[i-1]==mm.num[i]-1|mm.num[i+1]==mm.num[i]+1){
            TRUE
          } else {FALSE}
        }
      }
               }
  })
  )
  df.gap$group<-number <- paste(df.gap$grouped.gap, as.numeric(as.factor(df.gap$grouped.gap)), sep="_")
  df.gap$lenght.gap<-sapply(1:nrow(df.gap), FUN = function(i){
    if(df.gap$grouped.gap[i]==TRUE){
      nrow(subset(df.gap, df.gap$group==df.gap$group[i]))
    } else 1
  })
  subset(df.gap, df.gap$lenght.gap>=minimal.gap)$mm->mm.vraisgaps
  subset(df.gap, df.gap$lenght.gap<minimal.gap)$mm->mm.fauxgaps
  
  seqdata.trunc <- seqdata
  
  if (!is.na(left) & lc>0) {
    if (left=="DEL") seqdata.trunc[1:lc] <- void
    else if (left=="NEUTRAL") seqdata.trunc[1:lc] <- neutral
    else seqdata.trunc[1:lc] <- left
  }
  
  if (!is.na(right) & rc<=sl) {
    if (right=="DEL") seqdata.trunc[rc:sl] <- void
    else if (right=="NEUTRAL") seqdata.trunc[rc:sl] <- neutral
    else seqdata.trunc[rc:sl] <- right
  }
  
  if (!is.na(gaps) & length(mm>0)) {
    if (gaps=="DEL") seqdata.trunc[mm.vraisgaps] <- void
    else if (gaps=="NEUTRAL") seqdata.trunc[mm.vraisgaps] <- neutral
    else seqdata.trunc[mm.vraisgaps] <- gaps
  }
  seqdata.trunc2<-sapply(1:length(seqdata.trunc), FUN = function(i){
    if(i %in% mm.fauxgaps){
      if(regle.pour.faux.gap=="before"){
        match(TRUE, !is.na(seqdata.trunc[(i-1):1]))->rank
        replac.faux.gap<-as.character(seqdata.trunc[i-rank])
      }
      if(regle.pour.faux.gap=="after"){
        match(TRUE, !is.na(seqdata.trunc[(i+1):length(seqdata.trunc)]))->rank
        replac.faux.gap<-as.character(seqdata.trunc[i+rank])
      }
     replac.faux.gap
    } else seqdata.trunc[i]
  })
  names(seqdata.trunc2)<-names(seqdata.trunc)
  seqdata.trunc2->seqdata.trunc
  ndel <- sum(seqdata.trunc==void, na.rm=TRUE)
  
  if (ndel>0) {
    seqdata.trunc <- seqdata.trunc[seqdata.trunc!=void]
    seqdata.trunc <- c(seqdata.trunc,rep(void,ndel))
  }
  
  return(seqdata.trunc)
}
