#' Compute rmse of deconvolution
#'
#' @param deconvolution deconvolution results in long format
#' @param actual actual fractions in long format
#'
#' @return rmse as numeric value
#' @export
#'
#' @examples
#' \dontrun{
#' get_rmse(deconvolution, actual)
#'}

get_rmse <- function(deconvolution, actual){
  proportion_actual <- actual$proportion
  proportion_estimated <- deconvolution$proportion
  rmse <- sqrt(mean((proportion_actual - proportion_estimated)^2))
  rmse
}
