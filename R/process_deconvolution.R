#' Process cellanneal output
#'
#' @param cell_mix Output of a run_cellanneal_r call
#'
#' @return deconvolution in long format
#' @export
#'
#' @examples
#' 2 + 2

process_cellanneal <- function(cell_mix){
  tryCatch(
    expr = {
      cell_mix |> dplyr::select(!dplyr::starts_with("rho")) |>
        tidyr::pivot_longer(!sample, names_to = "cell_type",
                            values_to = "proportion")
      },
    error = function(e){
      stop("provide valid cellanneal result")
    }
  )
}


