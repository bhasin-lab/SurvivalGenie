os_data_compute = function(inputData, cp, cp_method, ind, selected){
  cp <- as.numeric(cp)
  labels <- colnames(inputData)[2:ind]
  i <- match(as.character(selected),labels)
  
  if(cp_method == "percentile"){
    low_cpi <- signif(cp[i][[1]],4)
    high_cpi <- signif(cp[i][[2]],4)
  } else {
    low_cpi <- signif(cp[i][[1]],4)
    high_cpi <- signif(cp[i][[1]],4)
  }
  end <- i+1
  gx <- as.matrix(as.numeric(inputData[,end]))
  rownames(gx) <- inputData$sample
  #identify samples within each low and high cut-points
  los = rownames(gx)[gx[,1] <= low_cpi]
  his = rownames(gx)[gx[,1] > high_cpi]
  inputData$group[inputData$sample %in% los] = "Low"
  inputData$group[inputData$sample %in% his] = "High"
  
  return(data=list(inputData=inputData, iSel=end))
}