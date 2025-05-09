# TODO: convert tree to internal dastaset once it's not changing any longer

cell_list <- list(
  # Top
  cells = c("leukocytes", "erythrocytes", "platelets", "uncharacterised", "progenitor_cells", "other", "doublet"),
  leukocytes = c("myeloid_cells", "mononuclear_leukocytes"),
  ### Myeloid
  myeloid_cells = c("granulocytes",  "mast_cells", "myeloid_progenitor_cells"),
  mast_cells = c("mast_cells_resting", "mast_cells_activated"),
  granulocytes = c("neutrophils", "basophils", "eosinophils"),
  ### mononuclear leukocytes
  mononuclear_leukocytes = c("lymphoid_cells",  "mononuclear_phagocytes"),

  mononuclear_phagocytes = c("dendritic_cells", "monocytes_macrophages"),
  dendritic_cells = c("lymphoid_dendritic_cells", "conventional_dendritic_cells"),
  conventional_dendritic_cells = c("conventional_dendritic_cells_resting", "conventional_dendritic_cells_activated", "conventional_dendritic_cells_axl+", "conventional_dendritic_cells_1", "conventional_dendritic_cells_2"),
  conventional_dendritic_cells_2 = c("conventional_dendritic_cells_2_1", "conventional_dendritic_cells_2_2"),
  lymphoid_dendritic_cells = c("lymphoid_dendritic_cells_axl+"),

  monocytes_macrophages = c("monocytes", "macrophages"),
  monocytes = c("monocytes_conventional", "monocytes_non_conventional"),
  macrophages = c("macrophages_M0", "macrophages_M1", "macrophages_M2"),


  ###ä Lymphoid cells
  lymphoid_cells = c("B_cells", "T_cells", "NK_cells", "innate_lymphoid_cells"),
  NK_cells = c("NK_cells_resting", "NK_cells_activated", "NK_CD56", "NK_cells_proliferating", "NK_cells_1", "NK_cells_2", "NK_cells_3", "NK_cells_4"),
  NK_CD56 = c("NK_CD56bright", "NK_CD56dim_CD57+", "NK_CD56dim_CD57-", "NK_CD56dim_CD57int", "NK_CD56dim_CD57low"),
  B_cells = c("B_cells_naive", "B_cells_memory", "Plasma_cells", "plasmablasts", "B_cells_activated", "B_cells_CD5", "B_cells_transitional"),
  B_cells_transitional = c("B_cells_transitional_kappa", "B_cells_transitional_lambda"),
  B_cells_memory = c("B_cells_memory_kappa", "B_cells_memory_lambda"),
  B_cells_naive = c("B_cells_naive_kappa", "B_cells_naive_lambda"),
  Plasma_cells = c("Plasma_cells_immature"),
  ### Bcells
  B_cells_memory = c("B_cells_atypical_memory", "B_cells_non_switched_memory", "B_cells_switched_memory"),
  B_cells_naive = c("B_cells_naive_IFN"),
  ### T cells
  T_cells = c("T_cells_CD4", "T_cells_CD8", "T_cells_other"),
  T_cells_other = c("T_cells_gd", "NKT_cells", "MAIT", "T_cells_DN"),
  T_cells_DN = c("T_cells_DN_1", "T_cells_DN_2"),
  T_cells_gd = c("T_cells_gd_1", "T_cells_gd_2", "T_cells_gd_3", "T_cells_gd_4"),
  ##### CD4
  T_cells_CD4 = c("T_regs", "T_CD4_non_regs"),
  T_CD4_non_regs =  c("T_cells_CD4_helper", "T_cells_CD4_naive", "T_cells_CD4_memory", "T_cells_CD4_terminal_effector", "T_cells_CD4_CTL", "T_cells_CD4_proliferating"),
  T_cells_CD4_helper = c("Th1", "Th1/Th17", "Th2", "Th17", "Th22", "T_cells_FH"),
  T_cells_CD4_naive = c("T_cells_CD4_naive_IFN"),
  T_cells_CD4_memory = c("T_cells_CD4_memory_resting", "T_cells_CD4_memory_activated", "T_cells_CD4_memory_exhausted", "T_cells_CD4_memory_HLA-DR+", "T_cells_CD4_terminal_effector_memory", "T_cells_CD4_central_memory"),
  T_cells_CD4_central_memory = c("T_cells_CD4_central_memory_1", "T_cells_CD4_central_memory_2", "T_cells_CD4_central_memory_3"),
  T_cells_CD4_terminal_effector_memory = c("T_cells_CD4_terminal_effector_memory_1","T_cells_CD4_terminal_effector_memory_2", "T_cells_CD4_terminal_effector_memory_3","T_cells_CD4_terminal_effector_memory_4"),
  T_regs = c("T_regs_KLRB_RORC", "T_regs_cytotoxic", "T_regs_memory", "T_regs_naive"),
  ##### CD8
  T_cells_CD8 = c("T_cells_CD8_naive", "T_cells_CD8_memory", "T_cells_CD8_HLA-DR+", "T_cells_CD8_proliferative", "T_cells_CD8_NKT_like"),
  T_cells_CD8_naive = c("T_cells_CD8_naive_IFN", "T_cells_CD8_naive_1", "T_cells_CD8_naive_2"),
  T_cells_CD8_memory = c("T_cells_CD8_central_memory", "T_cells_CD8_terminal_effector_memory", "T_cells_CD8_memory_KLRC2+", "T_cells_CD8_tissue_resident_memory"),
  T_cells_CD8_central_memory = c("T_cells_CD8_central_memory_CCR4+", "T_cells_CD8_central_memory_CCR4-", "T_cells_CD8_central_memory_GZMB+", "T_cells_CD8_central_memory_GZMK+", "T_cells_CD8_central_memory_1", "T_cells_CD8_central_memory_2", "T_cells_CD8_central_memory_3"),
  T_cells_CD8_terminal_effector_memory = c("T_cells_CD8_terminal_effector_memory_1", "T_cells_CD8_terminal_effector_memory_2", "T_cells_CD8_terminal_effector_memory_3", "T_cells_CD8_terminal_effector_memory_4", "T_cells_CD8_terminal_effector_memory_5", "T_cells_CD8_terminal_effector_memory_6"),
  ##### gd
  T_cells_gd = c("T_cells_gd_naive", "T_cells_gd_non_VD2", "T_cells_gd_VD2"),
  T_cells_gd_non_VD2 = c("T_cells_gd_VD1"),
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





