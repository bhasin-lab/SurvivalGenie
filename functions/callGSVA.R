#'@name callGSVA
#'@aliases callGSVA
#'@title GSVA enrichment analysis
#'@description Estimates GSVA enrichment zscores.
#'@usage callGSVA(x,y)
#'@param x A data frame or matrix of gene or probe expression values where rows corrospond to genes and columns corrospond to samples
#'@param y A list of genes as data frame or vector
#'@details This function uses "zscore" gene-set enrichment method in the estimation of gene-set enrichment scores per sample.
#'@return A gene-set by sample matrix of GSVA enrichment zscores.
#'@import GSVA
#'@examples 
#'g <- 10 ## number of genes
#'s <- 30 ## number of samples
#'## sample data matrix with values ranging from 1 to 10
#'rnames <- paste("g", 1:g, sep="")
#'cnames <- paste("s", 1:s, sep="")
#'expr <- matrix(sample.int(10, size = g*s, replace = TRUE), nrow=g, ncol=s, dimnames=list(rnames, cnames))
#'## genes of interest
#'genes <- paste("g", 1:g, sep="")
#'## Estimates GSVA enrichment zscores.
#'callGSVA(expr,genes)
#'@seealso GSVA
#'@export
callGSVA = function(x,y) {
    if(missing(x)){
    stop("input expression data missing!")
    }
    if(missing(y)){
    stop("input gene set missing!")
    }
    genes <- list(set1=y)
    gsva.results <- gsva(x, genes, method="zscore",verbose=FALSE, parallel.sz=2)
    tr_gsva.results <- t(gsva.results)
    colnames(tr_gsva.results) <- c("GSVA score")
    return (tr_gsva.results)
}