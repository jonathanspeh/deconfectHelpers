#' Turn results of deconvolution in long format
#'
#' @param cell_mix A dataframe containing a sample column and one column for each cell type
#'
#' @return deconvolution in long format
#' @export
#'
#' @examples
#' \dontrun{
#' pivot_longer_deconvolution(mixture)
#' }


pivot_longer_deconvolution <- function(cell_mix){
  tryCatch(
    expr = {
      cell_mix <- cell_mix |> dplyr::select(!dplyr::starts_with("rho")) |>
        tidyr::pivot_longer(!sample, names_to = "cell_type",
                            values_to = "proportion")
      },
    error = function(e){
      stop("provide valid deconvolution dataframe")
    }
  )
  cell_mix
}


#' Transpose the results of immunedeconv and turns into long format
#'
#' @param cell_mix The results of a immunedeconv deconvolution. Must contain columns for cell type and s row for each sample
#'
#' @return A dataframe containing sample, cell_type and proportions
#' @export
#'
#' @examples
#' \dontrun{
#' process_immunedeconv(mixture)
#' }

process_immunedeconv <- function(cell_mix){
  cell_mix_t <- t(cell_mix[-1])
  colnames(cell_mix_t) <- cell_mix$cell_type
  pivot_longer_deconvolution(dplyr::as_tibble(cell_mix_t, rownames = "sample"))
}




#' Funktion to unify the names of immunecells. Should be replaced
#'
#' @param cell_names A vector of immune cell names
#'
#' @return A vector of standardised cell names
#'
#' @export
#' @examples
#' fix_cell_names(c("NK","B.Naive", "B.Memory"), method = "abis")
#'
fix_cell_names_legacy <- function(cell_names){
  stopifnot("cell_names must be character vector" = is.character(cell_names))

  B_cells <- c("b cell", "bcell", "B_cell", "B.naive", "B.memory")
  T_cells_CD4 <- c("CD4", "cd4")
  T_cells_CD8 <- c("CD8", "cd8", "Cytotoxic T","ctl")
  T_regs <- c("T regulatory", "Treg", "regulatory t", "T reg", "T_reg")
  Monocytes <- c("mono", "classical monocytes")
  Neutrophils <- c("Neutro", "Neutrophiles", "Neutrophile granulocyte", "granulocyte neutro")
  Basophils <- c("Baso", "Basophile granulocyte")
  conventional_DCs <- c("conventional dendritic", "dendritic cells conv", "cDC",
                        "conventional dc","conventional_dc", "myeloid dendritic", "mDCs")
  plasmacytoid_DCs <- c("plasmacytoid dendritic", "pDC", "interferon-producing", "plasmacytoid")
  NK_cells <- c("natural killer cells", "NK cell", "NK_cell", "NK")
  T_gd_cells <- c("T.gd", "Tgd", "T gamma", "T_gamma")



  # patterns <- list(B_cells = B_cells, T_cells_CD4 = T_cells_CD4,
  #                  T_cells_CD8 = T_cells_CD8, T_regs = T_regs,
  #                  Monocytes = Monocytes, Neutrophils = Neutrophils,
  #                  conventional_DCs = conventional_DCs)

  cell_names_actual <- dplyr::case_when(
    grepl(paste(B_cells, collapse = "|"), cell_names, ignore.case = TRUE) ~ "B_cells",
    grepl(paste(T_cells_CD4, collapse = "|"), cell_names, ignore.case = TRUE) ~ "T_cells_CD4",
    grepl(paste(T_cells_CD8, collapse = "|"), cell_names, ignore.case = TRUE) ~ "T_cells_CD8",
    grepl(paste(T_regs, collapse = "|"), cell_names, ignore.case = TRUE) ~ "T_regs",
    grepl(paste(Monocytes, collapse = "|"), cell_names, ignore.case = TRUE) ~ "monocytes",
    grepl(paste(Basophils, collapse = "|"), cell_names, ignore.case = TRUE) ~ "basophils",
    grepl(paste(Neutrophils, collapse = "|"), cell_names, ignore.case = TRUE) ~ "neutrophils",
    grepl(paste(conventional_DCs, collapse = "|"), cell_names, ignore.case = TRUE) ~ "conventional_dendritic_cells",
    grepl(paste(plasmacytoid_DCs, collapse = "|"), cell_names, ignore.case = TRUE) ~ "lymphoid_dendritic_cells",
    grepl(paste(NK_cells, collapse = "|"), cell_names, ignore.case = TRUE) ~ "NK_cells",
    grepl(paste(T_gd_cells, collapse = "|"), cell_names, ignore.case = TRUE) ~ "T_gd_cells",
    .default =  paste("Other, original was: ", cell_names))
  cell_names_actual
}



#' Funktion for method-specific cell-type reannotation
#'
#' @param cell_names A character vector containin cell_types
#' @param method  A character specifying the deconovlution method that has been used
#'
#' @return A Vector containing the generalised Cell-Types or NA for unknown types
#' @export
#'
#' @examples
#' fix_cell_names(c("NK","B.Naive", "B.Memory"), method = "abis")
#'
fix_cell_names <- function(cell_names, method){
  if(!method%in%cell_types$method) stop(paste0("Method must be one of: ", paste(unique(cell_types$method), collapse = ", ")))
  cell_types <- dplyr::filter(cell_types, method == method)
  newnames <- cell_types$general_name[match(cell_names, cell_types$method_name)]
  # If there are T_regs and T_cells_CD4 in dataset, T_cells_CD4 has the be renamed to non_regs since T_cell_CD4 is parent of T_reg
  if("T_regs" %in% newnames & "T_cells_CD4" %in% newnames) newnames[newnames == "T_cells_CD4"] <- "T_CD4_non_regs"
  newnames
}






