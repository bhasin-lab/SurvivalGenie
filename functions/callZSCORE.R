#'@name callZSCORE
#'@aliases callZSCORE
#'@title Row ZSCORES
#'@description Estimates the zscores for each row in the data matrix
#'@usage callZSCORE(x)
#'@param x A data frame or matrix of gene or probe expression values where rows corrospond to genes and columns corrospond to samples
#'@details This function compute row zscores per sample when number of genes is less than 3
#'@return A gene-set by sample matrix of zscores.
#'@examples 
#'g <- 2 ## number of genes
#'s <- 60 ## number of samples
#'## sample data matrix with values ranging from 1 to 10
#'rnames <- paste("g", 1:g, sep="")
#'cnames <- paste("s", 1:s, sep="")
#'expr <- matrix(sample.int(10, size = g*s, replace = TRUE), nrow=g, ncol=s, dimnames=list(rnames, cnames))
#'## Estimates zscores
#'callZSCORE(expr)
#'@export
callZSCORE = function(x) {
    if(missing(x)){
    stop("input expression data missing!")
    }
    #compute row zscores
    zscores_results <- data.frame(colnames(x),scale(t(x)),row.names=NULL)
    colnames(zscores_results)[1] <- "samples"
    #compute the added zscores, as applicable and rename the column names
    if(ncol(zscores_results)==2){
      colnames(zscores_results)[2] <- "zscore"
    }else{
      zscores_results$zscores <- zscores_results[,2] + zscores_results[,3]
      zscores_results <- zscores_results[,-c(2,3)]
    }
    return (zscores_results)
}