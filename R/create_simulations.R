#' Helper to create a simulated sc-set including annotation
#'
#' @param n_genes number of genes in dataset
#' @param n_cells_est estimated number of cells, will only be exact if multiple of six
#' @param make_signature boolean weather to make a signature and write it to disk
#' @param signature_path path for signature matrix
#'
#' @return a list containing the counts and annotaiton
#' @export
#'
#' @examples
#' simulate_sc(n_genes = 100, n_cells_est = 30, make_signature = FALSE)

simulate_sc <- function(n_genes = 10000, n_cells_est = 300,
                        make_signature = TRUE, signature_path = "./signature.csv"){
  n1 <- round(n_cells_est/6)
  n2 <- round(n_cells_est/6)
  n3 <- round(n_cells_est/3)
  n4 <- round(n_cells_est/30)
  n5 <- round(n_cells_est/5)
  n6 <- round(n_cells_est/10)

  n_cells_actual <- n1 + n2 + n3 + n4 + n5 + n6

  counts <- Matrix::Matrix(cbind(
    replicate(n1, rpois(n_genes, 0.01)),
    replicate(n2, rpois(n_genes, 0.02)),
    replicate(n3, rpois(n_genes, 0.06)),
    replicate(n4, rpois(n_genes, 0.1)),
    replicate(n5, rpois(n_genes, 0.05)),
    replicate(n6, rpois(n_genes, 0.065))), sparse = TRUE)

  colnames(counts) <- paste0("cell_", rep(1:n_cells_actual))
  rownames(counts) <- paste0("gene_", rep(1:n_genes))

  annotation <- data.frame(
    "ID" = paste0("cell_", rep(1:n_cells_actual)),
    "cell_type" = c(
      rep("T cells CD4", n1),
      rep("T cells CD8", n2),
      rep("Macrophages", n3),
      rep("NK cells", n4),
      rep("B cells", n5),
      rep("Monocytes", n6)))

  if(make_signature){

    signature <- dplyr::as_tibble(as.matrix(counts), rownames = "gene") |>
      tidyr::pivot_longer(!"gene") |>
      dplyr::left_join(annotation, by = dplyr::join_by("name" == "ID")) |>
      dplyr::summarise(mean = mean(.data$value), .by = c("gene", "cell_type")) |>
      tidyr::pivot_wider(names_from = "cell_type", values_from = "mean")


    colnames(signature)[1] <- "index"

    data.table::fwrite(signature, signature_path)
  }
  return(list(counts = counts, annotation = annotation))
}








