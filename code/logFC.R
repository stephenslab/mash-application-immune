# ----- logFC -----
logFC.compare = function(groupname, thresh){
  result = c()
  group = c()
  sti.data = processed.data[[groupname]]

  # eQTL base only
  base.only = setdiff(eQTL.index.mash$base, eQTL.index.mash[[groupname]])
  sig = which(resEZ$result$lfsr[base.only, 1] < thresh)
  base.index = base.only[sig]
  base=which(gene.names.pro %in% gene.names.data[base.index])

  n = length(base)
  group = c(group, rep('eQTLBase', n))

  # base
  base.mean = apply(processed.data$ctrl[base, 5:138],1 ,mean, na.rm=TRUE)
  # sti
  sti.mean = apply(sti.data[base, 5:138],1 ,mean, na.rm=TRUE)

  result = c(result, log2(sti.mean/base.mean))

  # sti only
  sti.only = setdiff(eQTL.index.mash[[groupname]], eQTL.index.mash$base)
  sig = which(resEZ$result$lfsr[sti.only,groupname] < thresh)
  sti.index = sti.only[sig]
  sti = which(gene.names.pro %in% gene.names.data[sti.index])

  n = length(sti)
  group = c(group, rep('eQTLSti', n))
  # base
  base.mean = apply(processed.data$ctrl[sti, 5:138],1 ,mean, na.rm=TRUE)
  # sti
  sti.mean = apply(sti.data[sti, 5:138],1 ,mean, na.rm=TRUE)

  result = c(result, log2(sti.mean/base.mean))

  return(data.frame(logFC=result, group=factor(group, c('eQTLBase', 'eQTLSti'))))
}

give.n <- function(x){
  return(c(y = max(x)*1.1, label = length(x)))
  # experiment with the multiplier to find the perfect position
}
