# TODO: convert tree to internal dastaset once it's not changing any longer

cell_list <- list(
  # Top
  cells = c("immune_cells", "non_immune_cells", "uncharacterised", "progenitor_cells"),
  immune_cells = c("myeloid_cells", "lymphoid_cells"),
  ### Myeloid
  myeloid_cells = c("conventional_dendritic_cells", "monocytes_macrophages",
                    "granulocytes",  "mast_cells", "myeloid_progenitor_cells"),
  mast_cells = c("mast_cells_resting", "mast_cells_activated"),
  conventional_dendritic_cells = c("conventional_dendritic_cells_resting", "conventional_dendritic_cells_activated"),
  monocytes_macrophages = c("monocytes", "macrophages"),
  monocytes = c("monocytes_conventional", "monocytes_non_conventional"),
  macrophages = c("macrophages_M0", "macrophages_M1", "macrophages_M2"),
  granulocytes = c("neutrophils", "basophils", "eosinophils"),
  ### Lymphoid
  lymphoid_cells = c("B_cells", "T_cells", "NK_cells", "lymphoid_dendritic_cells"),
  NK_cells = c("NK_cells_resting", "NK_cells_activated", "NK_CD56", "NK_cells_proliferating"),
  NK_CD56 = c("NK_CD56bright", "NK_CD56dim_CD57+", "NK_CD56dim_CD57-", "NK_CD56dim_CD57int", "NK_CD56dim_CD57low"),
  B_cells = c("B_cells_naive", "B_cells_memory", "Plasma_cells", "plasmablasts", "B_cells_activated", "B_cells_CD5", "B_cells_transitional"),
  ### Bcells
  B_cells_memory = c("B_cells_atypical_memory", "B_cells_non_switched_memory", "B_cells_switched_memory"),
  B_cells_naive = c("B_cells_naive_IFN"),
  ### T cells
  T_cells = c("T_cells_CD4", "T_cells_CD8", "T_cells_gd", "NKT_cells", "MAIT", "T_cells_DN"),
  ##### CD4
  T_cells_CD4 = c("T_regs", "T_CD4_non_regs"),
  T_CD4_non_regs =  c("T_cells_CD4_helper", "T_cells_CD4_naive", "T_cells_CD4_memory", "T_cells_CD4_terminal_effector"),
  T_cells_CD4_helper = c("Th1", "Th1/Th17", "Th2", "Th17", "Th22", "T_cells_FH"),
  T_cells_CD4_naive = c("T_cells_CD4_naive_IFN"),
  T_cells_CD4_memory = c("T_cells_CD4_memory_resting", "T_cells_CD4_memory_activated", "T_cells_CD4_memory_exhausted", "T_cells_CD4_memory_HLA-DR+", "T_cells_CD4_terminal_effector_memory"),
  T_regs = c("T_regs_KLRB_RORC", "T_regs_cytotoxic", "T_regs_memory", "T_regs_naive"),
  ##### CD8
  T_cells_CD8 = c("T_cells_CD8_naive", "T_cells_CD8_memory", "T_cells_CD8_HLA-DR+", "T_cells_CD8_proliferative"),
  T_cells_CD8_naive = c("T_cells_CD8_naive_IFN"),
  T_cells_CD8_memory = c("T_cells_CD8_central_memory", "T_cells_CD8_terminal_effector_memory", "T_cells_CD8_memory_KLRC2+", "T_cells_CD8_tissue_resident_memory"),
  T_cells_CD8_central_memory = c("T_cells_CD8_central_memory_CCR4+", "T_cells_CD8_central_memory_CCR4-", "T_cells_CD8_central_memory_GZMB+", "T_cells_CD8_central_memory_GZMK+"),
  ##### gd
  T_cells_gd = c("T_cells_gd_naive", "T_cells_gd_VD1", "T_cells_gd_VD2"),
  T_cells_gd_VD1 = c("T_cells_gd_VD1_GZMB+", "T_cells_gd_VD1_GZMK+"),
  T_cells_gd_VD2 = c("T_cells_gd_VD2_GZMB+", "T_cells_gd_VD2_GZMK+")
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





