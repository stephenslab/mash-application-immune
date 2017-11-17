# ---- subset.data ----
subset.data = function(data, subset){
  data.subset = data
  data.subset$Bhat = data$Bhat[subset,]
  data.subset$Shat = data$Shat[subset,]
  data.subset$Shat_alpha = data$Shat_alpha[subset,]
  data.subset
}
