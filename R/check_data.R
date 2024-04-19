#' Find gene names that excel converte to dates
#'
#' @param dat A data frame containing gene names as a column.
#' @param gene_names A string specifying the column to check.
#' @param index  A bool indicateing wether to return the column index of suspicious genes. Default is TRUE
#'
#' @return A tibble containing the suspicious genes
#' @export
#'
#' @examples
#' 2 + 2
find_suspicious_names <- function(dat, gene_names, index = TRUE){
  stopifnot("gene_names must be a valid column name" = gene_names %in% colnames(dat),
            "index must be logical" = is.logical(index))
  if(index){
    dat <- tibble::rowid_to_column(dat, "row_index")
  }
  suppressWarnings({
    susp_names <- dat[!is.na(as.numeric(dat[[gene_names]])),]
  })
  if(nrow(susp_names) == 0) message("no suspicious gene names found")
  else susp_names
}
