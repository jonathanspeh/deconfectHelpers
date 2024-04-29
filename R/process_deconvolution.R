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




#' Funktion to unify the names of immunecells
#'
#' @param cell_names A vector of immune cell names
#'
#' @return A vector of standardised cell names
#' @export
#'
#' @examples
#' fix_cell_names(c("bcells", "T cells CD4", "T reg"))
#'
fix_cell_names <- function(cell_names){
  stopifnot("cell_names must be character vector" = is.character(cell_names))

  B_cells <- c("b cell", "bcell", "B_cell")
  T_cells_CD4 <- c("CD4", "cd4")
  T_cells_CD8 <- c("CD8", "cd8", "Cytotoxic T","ctl")
  T_regs <- c("T regulatory", "Treg", "regulatory t", "T reg", "T_reg")
  Monocytes <- c("mono", "classical monocytes")
  Neutrophils <- c("Neutro", "Neutrophiles", "Neutrophile granulocyte", "granulocyte neutro")
  conventional_DCs <- c("conventional dendritic", "dendritic cells conv", "cDC",
                        "conventional dc","conventional_dc", "myeloid dendritic")
  NK_cells <- c("natural killer cells", "NK cell", "NK_cell")

  # patterns <- list(B_cells = B_cells, T_cells_CD4 = T_cells_CD4,
  #                  T_cells_CD8 = T_cells_CD8, T_regs = T_regs,
  #                  Monocytes = Monocytes, Neutrophils = Neutrophils,
  #                  conventional_DCs = conventional_DCs)

  cell_names_actual <- dplyr::case_when(
    grepl(paste(B_cells, collapse = "|"), cell_names, ignore.case = TRUE) ~ "B_cells",
    grepl(paste(T_cells_CD4, collapse = "|"), cell_names, ignore.case = TRUE) ~ "T_cells_CD4",
    grepl(paste(T_cells_CD8, collapse = "|"), cell_names, ignore.case = TRUE) ~ "T_cells_CD8",
    grepl(paste(T_regs, collapse = "|"), cell_names, ignore.case = TRUE) ~ "T_regs",
    grepl(paste(Monocytes, collapse = "|"), cell_names, ignore.case = TRUE) ~ "Monocytes",
    grepl(paste(Neutrophils, collapse = "|"), cell_names, ignore.case = TRUE) ~ "Neutrophils",
    grepl(paste(conventional_DCs, collapse = "|"), cell_names, ignore.case = TRUE) ~ "conventional_DCs",
    grepl(paste(NK_cells, collapse = "|"), cell_names, ignore.case = TRUE) ~ "NK_cells",
    .default =  paste("Other, original was: ", cell_names))
  cell_names_actual
  }



