#' @author Elie
#' @description  ...
#' @title La fonction "seqdef_modgap"
#' @description  seqdef_modgap: permet de définir un nombre minimal de gaps consécutif pour êtres considérés comme des vrais gaps. Prend en compte tous les paramètres de TraMineR::seqdef() avec deux argumets supplémentaires. 
#' Attention: l'argument gap ne doit pas être nul. 
#' Voir les modifs dans seqprep.modgap() (file: seqprep_modgap.R) et dans TraMineR.trunc.modgap() (file: TraMineR_trunc_modgap.R)
#' @param minimal.gap numeric. nombre minimum d'états manquant consécutifs pouvant être considérés comme un "vrai" gap (une sortie du RSA)
#' @param regle.pour.faux.gap Choix entre: c("before", "after"). Si "before" ("after"), les faux gap sont recodés avec l'état précédent (suivant) non manquant.  
#' @return un vecteur de facteurs res
#' @export

seqdef_modgap<-function (data, var = NULL, informat = "STS", stsep = NULL, alphabet = NULL, 
          states = NULL, id = NULL, weights = NULL, start = 1, left = NA, 
          right = "DEL", gaps = "gap", missing = NA, void = "%", nr = "*", 
          cnames = NULL, xtstep = 1, tick.last = FALSE, cpal = NULL, 
          missing.color = "darkgrey", labels = NULL, minimal.gap=3, regle.pour.faux.gap="before", ...) 
{
  maxstatedisplay <- 12
  if (!is.character(void) || void %in% c(left, gaps, right)) 
    msg.stop("'void' must be a character different from left, gaps, and right!")
  seqdata <- seqxtract(data, var, data.frame = TRUE)
  if (informat == "STS") {
    if (is.null(stsep)) {
      sf <- seqfcheck(seqdata)
      if (sf %in% c("-", ":")) 
        seqdata <- seqdecomp(seqdata, sep = sf)
      else if (sf == "-X") 
        message(" [!] found '-' character in state codes, not recommended")
    }
    else {
      seqdata <- seqdecomp(seqdata, sep = stsep)
    }
  }
  else if (informat %in% c("SPS", "SPELL")) {
    if (is.na(missing)) 
      missing <- "_!NA!_"
    seqdata <- seqformat(seqdata, from = informat, to = "STS", 
                         stsep = stsep, missing = missing, ...)
    missing <- NA
  }
  if (any(is.logical(seqdata))) {
    for (i in 1:ncol(seqdata)) seqdata[, i] <- as.character(seqdata[, 
                                                                    i])
  }
  cntmp <- colnames(seqdata)
  statl <- seqstatl(seqdata)
  if (!is.na(missing)) {
    statl <- statl[!statl == missing & !statl == void]
  }
  if (is.null(alphabet)) {
    plevels <- as.character(statl)
  }
  else {
    if (any(statl %in% alphabet == FALSE)) {
      stop("\n [!] one or more states appearing in the data not included in 'alphabet'", 
           call. = FALSE)
    }
    plevels <- alphabet
  }
  seqdata <- as.matrix(seqdata)
  if (!is.null(states)) {
    for (i in 1:length(plevels)) {
      seqdata[seqdata == plevels[i]] <- states[i]
    }
  }
  ####
  gapmin<-minimal.gap; fauxgapcode<-regle.pour.faux.gap
  ###
  if ((is.na(missing) && any(is.na(seqdata))) || ((!is.na(missing)) && 
                                                  any(seqdata == missing, na.rm = TRUE))) {
    message(" [>] found missing values ('", missing, "') in sequence data")
    seqdata.code <- seqprep.modgap(seqdata, left = left, gaps = gaps, 
                       right = right, missing = missing, void = void, nr = nr, 
                       minimal.gap=minimal.gap, regle.pour.faux.gap=regle.pour.faux.gap)
  }
  seqdata <- as.data.frame(seqdata.code)
  class(seqdata) <- c("stslist", "data.frame")
  if (is.null(start)) 
    start <- 1
  attr(seqdata, "start") <- start
  attr(seqdata, "missing") <- missing
  attr(seqdata, "void") <- void
  attr(seqdata, "nr") <- nr
  if (missing(states)) {
    nbdatastat <- length(statl)
    message(" [>] ", nbdatastat, " distinct states appear in the data: ")
    for (i in 1:min(nbdatastat, maxstatedisplay)) {
      message("     ", i, " = ", statl[i])
    }
    if (nbdatastat > maxstatedisplay) 
      message("      ...")
    A <- plevels
  }
  else {
    A <- states
  }
  specst <- NULL
  if (!is.na(left) && left != "DEL" && left != "NEUTRAL" && 
      !(left %in% A)) {
    specst <- c(specst, left)
  }
  if (!is.na(gaps) && gaps != "DEL" && gaps != "NEUTRAL" && 
      !(gaps %in% A)) {
    specst <- c(specst, gaps)
  }
  if (!is.na(right) && right != "DEL" && right != "NEUTRAL" && 
      !(right %in% A)) {
    specst <- c(specst, right)
  }
  if (!is.null(specst)) {
    message(" [>] adding special state(s) to the alphabet: ", 
            paste(specst, collapse = "/"))
    plevels <- c(plevels, specst)
    A <- c(A, specst)
  }
  attr(seqdata, "alphabet") <- A
  nbstates <- length(A)
  if (nbstates > 12 && missing(cpal)) 
    warning(" [!] No color palette automatically assigned because number of states > 12.\n               \n     Use 'cpal' argument to assign one.", 
            call. = FALSE)
  for (i in 1:ncol(seqdata)) {
    seqdata[, i] <- factor(seqdata[, i], levels = c(A, nr, 
                                                    void))
  }
  if (!is.null(labels)) {
    if (length(labels) != nbstates) {
      stop("\n [!] number of labels must equal number of states in the alphabet", 
           call. = FALSE)
    }
  }
  else labels <- A
  attr(seqdata, "labels") <- labels
  message(" [>] state coding:")
  nc1 <- max(nchar(plevels) + 1, 12)
  nc2 <- max(nchar(A) + 1, 9)
  message("       ", format("[alphabet]", width = nc1), format("[label]", 
                                                               width = nc2), "[long label] ")
  for (i in 1:min(nbstates, maxstatedisplay)) {
    message("     ", i, "  ", format(plevels[i], width = nc1), 
            format(A[i], width = nc2), labels[i])
  }
  if (nbstates > 12) {
    message("      ... (", nbstates, " states)")
  }
  if (!is.null(weights)) {
    if (length(weights) != nrow(seqdata)) {
      stop("\n [!] number of weights must equal number of sequences", 
           call. = FALSE)
    }
    message(" [>] sum of weights: ", round(sum(weights), 
                                           2), " - min/max: ", min(weights), "/", max(weights))
    names(weights) <- rownames(seqdata)
  }
  attr(seqdata, "weights") <- weights
  if (is.null(cpal)) {
    if (nbstates <= 2) 
      attr(seqdata, "cpal") <- brewer.pal(3, "Accent")[1:nbstates]
    else if (nbstates <= 8) 
      attr(seqdata, "cpal") <- brewer.pal(nbstates, "Accent")
    else if (nbstates > 8 & nbstates <= 12) 
      attr(seqdata, "cpal") <- brewer.pal(nbstates, "Set3")
    else if (nbstates > 12) 
      message(" [>] no color palette attributed, provide one to use graphical functions")
  }
  else {
    if (length(cpal) != nbstates) 
      stop("\n [!] number of colors in 'cpal' must equal length of alphabet", 
           call. = FALSE)
    else attr(seqdata, "cpal") <- cpal
  }
  attr(seqdata, "missing.color") <- missing.color
  nbseq <- nrow(seqdata)
  seql <- seqlength(seqdata)
  message(" [>] ", nbseq, " sequences in the data set")
  message(" [>] min/max sequence length: ", min(seql), "/", 
          max(seql))
  if (!is.null(cnames)) {
    colnames(seqdata) <- cnames
  }
  else {
    if (is.null(cntmp)) 
      colnames(seqdata) <- paste("T", start:(max(seql) + 
                                               start - 1), sep = "")
    else if (all(is.na(cntmp) != TRUE)) 
      colnames(seqdata) <- cntmp
    else colnames(seqdata) <- paste("T", start:(max(seql) + 
                                                  start - 1), sep = "")
  }
  attr(seqdata, "xtstep") <- xtstep
  attr(seqdata, "tick.last") <- tick.last
  if (!is.null(id)) {
    if (length(id) == 1 && id == "auto") {
      rownames(seqdata) <- paste("[", 1:nbseq, "]", sep = "")
    }
    else {
      rownames(seqdata) <- id
    }
    if (!is.null(weights)) {
      names(weights) <- rownames(seqdata)
    }
  }
  descr <- packageDescription("TraMineR")
  attr(seqdata, "Version") <- descr$Version
  return(seqdata)
}
