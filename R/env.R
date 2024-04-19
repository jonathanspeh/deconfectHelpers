deconv_env <- rlang::env()

#' run_cellanneal_r
#'
#' Wrapper for cellaneals "make_gene_dictionary" and "deconvolve" funktions.
#' Takes paths of signature and mixture file and returns dataframe with fractions
#' cellanneal has been developed by Lisa Buchauer, see https://github.com/LiBuchauer/cellanneal for mor information
#'
#'
#'
#' @param signature Path to the signature csv-file
#' @param mixture Path to the mixture csv-file
#' @param disp_min Minimum sclaed dispersion, defaults to 0.5
#' @param bulk_min Minimum expression in mixture, defaults to 1e-5
#' @param bulk_max Maximum expression in mixture, defualts to 0.01
#' @param maxiter max number of iteration, defaults to 1000
#'
#' @return A dataframe containing the results of celltype deconvolution
#' @export
#'
#' @examples
#'\dontrun{
#' run_cellanneal_r(path_to_signature, path_to_mixture)
#'}

run_cellanneal_r <- function(signature, mixture, disp_min = 0.5, bulk_min = 1e-5, bulk_max = 0.01, maxiter = 1000){
  #reticulate::use_condaenv("cellanneal")
  all_mix <- deconv_env$run_cellanneal(signature, mixture, disp_min = disp_min,
                            bulk_min = bulk_min, bulk_max = bulk_max,
                            maxiter = maxiter)
  sample <- rownames(all_mix)
  cbind(sample, as.data.frame(all_mix, row.names = NULL))
}
