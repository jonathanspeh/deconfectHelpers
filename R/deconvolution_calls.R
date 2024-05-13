#' A function to call the abis deconvolution method
#'
#' @param mixture Bulkmatrix with rownames for each gene and colnames for each sample
#' @param use_build_in_signature Boolean weather to use the signature matrix provided by the abis authors
#' @param custom_signature An alternative signature to use. Needs to have genes as rownames and cell types as columns
#'
#' @return A Matrix containing the results of deconvolution
#' @export
#'
#' @examples
#'\dontrun{
#' run_abis(mixture)
#'}

run_abis <- function(mixture, use_build_in_signature = TRUE, custom_signature = NULL){
  if(!use_build_in_signature)  abis_signature = custom_signature

  genes <- intersect(rownames(mixture), rownames(abis_signature))

  deconvolution <- (apply(mixture[genes, , drop=F], 2,
                          function(x) stats::coef(MASS::rlm(as.matrix(abis_signature[genes,]), x, maxit =100 ))))

  deconvolution
}


