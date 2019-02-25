methods.args<-list(
  "edit"=list(
  "OM"=c("sm", "indel", "norm", "refseq"),
"OMloc"=c("sm", "expcost", "context", "refseq"),
"OMslen"=c("sm", "indel", "link", "h", "refseq"),
"OMspell"=c("sm", "indel", "tpow", "expcost", "refseq"),
"OMstran"=c("sm", "indel", "transindel", "otto", "previous", "add.column", "weighted"),
"HAM"=c("sm", "norm", "refseq"),
"DHD"=c("sm", "norm", "refseq"),
"TWED"=c("sm", "indel", "h", "nu", "refseq")),
"common_attributes"=list(
"LCS"=c("norm", "refseq"), 
"LCP"=c("norm", "refseq"), 
"RLCP"=c("norm", "refseq"),
"NMS"=c("prox", "kweights", "refseq"),
"NMSMST"=c("kweights", "tpow", "refseq"),
"SVRspell"=c("prox", "kweights", "tpow", "refseq")),
"distrib"=list(
"CHI2"=c("breaks", "step", "overlap", "weighted", "norm"),
"EUCLID"=c("breaks", "step", "overlap", "weighted", "norm"))
)
method.input<-list(
  "sm"
)
save(methods.args, file="data/method_args.RData")
