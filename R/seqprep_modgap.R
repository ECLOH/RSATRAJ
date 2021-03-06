seqprep.modgap <- function(seqdata, left=NA, right="DEL", gaps=NA, 
                    neutral="#", missing=NA, void="%", nr="*", minimal.gap, regle.pour.faux.gap) {
  
  nbseq <- nrow(seqdata)
  sl <- ncol(seqdata)
  
  message(" [>] preparing ",nbseq, " sequences")
  message(" [>] coding void elements with '", void, "' and missing values with '", nr,"'")
  
  if (is.na(missing)) {
    mstate <- is.na(seqdata)
  } else {
    mstate <- seqdata==missing
  }
  
  allmiss <- NULL
  for (i in 1:nbseq) {
    nbmiss <- sum(mstate[i,], na.rm=TRUE)
    if (nbmiss>0 && nbmiss<sl) {
      #print(paste("on est au numéro", i))
      seqdata[i,] <- TraMineR.trunc.modgap(seqdata=seqdata[i,], mstate=mstate[i,], sl=sl,
                                    left=left, right=right, gaps=gaps,
                                    neutral=neutral, void=void, minimal.gap=minimal.gap, regle.pour.faux.gap=regle.pour.faux.gap)
    }
    else if (nbmiss==sl) {
      allmiss <- c(allmiss,i)
    }
  }
  
  if (length(allmiss)>0) {
    message(" [!] sequence with index: ", paste(allmiss, collapse=","), " contains only missing values.\n     This may produce inconsistent results.")
  }
  
  ## Setting a new code for missing statuses
  if (is.na(missing)) seqdata[is.na(seqdata)] <- nr
  else seqdata[seqdata==missing] <- nr
  
  return(seqdata)
} 