# TODO: convert tree to internal dastaset once it's not changing any longer

cell_list <- list(
  # Top
  cells = c("immune_cells", "non_immune_cells", "uncharacterised"),
  immune_cells = c("myeloid_cells", "lymphoid_cells"),
  ### Myeloid
  myeloid_cells = c("conventional_dendritic_cells", "monocytes_macrophages",
                    "granulocytes",  "mast_cells", "progenitor_cells"),
  mast_cells = c("mast_cells_resting", "mast_cells_activated"),
  conventional_dendritic_cells = c("conventional_dendritic_cells_resting", "conventional_dendritic_cells_activated"),
  monocytes_macrophages = c("monocytes", "macrophages"),
  monocytes = c("monocytes_conventional", "monocytes_non_conventional"),
  macrophages = c("macrophages_M0", "macrophages_M1", "macrophages_M2"),
  granulocytes = c("neutrophils", "basophils", "eosinophils"),
  ### Lymphoid
  lymphoid_cells = c("B_cells", "T_cells", "NK_cells", "lymphoid_dendritic_cells"),
  NK_cells = c("NK_cells_resting", "NK_cells_activated"),
  B_cells = c("B_cells_naive", "B_cells_memory", "Plasma_cells", "plasmablasts"),
  T_cells = c("T_cells_CD4", "T_cells_CD8", "T_cells_gd", "NKT_cells", "MAIT", "T_cells_DN"),
  T_cells_CD4 = c("T_regs", "T_CD4_non_regs"),
  T_CD4_non_regs =  c("Th1", "Th2", "Th17", "T_cells_FH", "T_cells_CD4_naive", "T_cells_CD4_memory"),
  T_cells_CD4_memory = c("T_cells_CD4_memory_resting", "T_cells_CD4_memory_activated"),
  T_cells_CD8 = c("T_cells_CD8_naive", "T_cells_CD8_memory")
)

cell_df <- dplyr::tibble(parent = names(cell_list), child = cell_list) |>
  tidyr::unnest_longer(child)

#' Create a hierarchic data tree from deconvolution results
#'
#' @param cell_mix A data frame containing sample, cell_type and proportion as columns.
#'
#' @return A data.tree object were each sample is one attribute
#' @export
#'
#' @examples
#' \dontrun{
#' maxe_cell_tree(mixture)
#' }
#'
make_cell_tree <- function(cell_mix){
  cell_mix_wide <- tidyr::pivot_wider(cell_mix,
                                      names_from = "sample",
                                      values_from = "proportion")
  join_df <-  dplyr::left_join(cell_df, cell_mix_wide, by = dplyr::join_by("child" == "cell_type"))
  data.tree::FromDataFrameNetwork(join_df)
  }




#' A function to get cumulative proportions of cell types in a hierarchic tree
#'
#' @param node A data.tree object with cell types and samples as attributes
#' @param return_all A boolean to specify whether original and cumulative proportions. Appends _cumulative to samples if true
#'
#' @return A dataframe with one row per cell type and columns for samples. Contains two columns per sample if return_all is TRUE
#' @export
#'
#' @examples
#' \dontrun{
#' get_cumulative_proportions(cell_tree)
#' }
get_cumulative_proportions <- function(node, return_all = FALSE){
  # Define function to recursively calculate column names
  cumulate_sums <- function(x, name) {
    new_name <- ifelse(return_all, paste0(name, "_cumulative"), name)
    x[[new_name]] <-
      sum(c(x[[name]], purrr::map_dbl(x$children, cumulate_sums, name)), na.rm = TRUE)
  }
  # get attributes as names

  names <- node$attributesAll # can be replaced if this fails / throws warnings
  # loop over attributes to compute cumulative sums
  for(i in seq_along(names)) {
    cumulate_sums(node, names[i])
  }

  # Get data_frame with cumulative sums
  tree_df <- data.tree::ToDataFrameNetwork(node)
  names <- node$attributesAll
  for(i in seq_along(names)){
    name = names[i]
    tree_df <- cbind(tree_df, data.tree::ToDataFrameNetwork(node, names[i])[,3])
    colnames(tree_df)[i+2] <- name
  }
  dplyr::select(tree_df, !"from", "cell_type"="to")
}





