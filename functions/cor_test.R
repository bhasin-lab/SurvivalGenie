cor.test.p <- function(x){
  get_pval <- function(x, y) cor.test(x, y)[["p.value"]]
  pvals <- outer(
    colnames(x), 
    colnames(x), 
    Vectorize(function(i,j) get_pval(x[,i], x[,j]))
  )
  dimnames(pvals) <- list(colnames(x), colnames(x))
  return(pvals)
}