#' Plot Violin
#'
#' @param deconvolution deconvolution results in long format
#' @param actual actual fractions in long format
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples
#' \dontrun{
#' plot_deconvolution_violin(deconvultion, actual)
#'}

plot_deconvolution_violin <- function(deconvolution, actual){
  deconvolution <- dplyr::mutate(deconvolution, flag = "estimated")
  actual <- dplyr::mutate(actual, flag = "actual")
  rbind(deconvolution, actual) |>
    dplyr::mutate(group = paste(cell_type, flag, sep = "_")) |>
      ggplot2::ggplot(ggplot2::aes(x = cell_type, y = proportion, colour = flag)) +
      ggplot2::geom_violin() +
      ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(n.dodge = 2))
}


#' Internal plot function
#'
#' @param cell_type_filter a vector of celltypes to plot
#'
#' @return a plot of the selected celltype
#'

plot_corr_celltype <- function(est_v_actual, cell_type_filter) {
  subs <- est_v_actual |>
    dplyr::filter(cell_type == cell_type_filter)
  min <- min(min(subs$proportion_estimate),min(subs$proportion_actual))
  max <- max(max(subs$proportion_estimate),max(subs$proportion_actual))
  lim <- c(min, max)
  subs |>
    ggplot2::ggplot(ggplot2::aes(x = proportion_estimate, y = proportion_actual)) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method = "lm") +
    ggplot2::ggtitle(cell_type_filter) +
    ggplot2::coord_cartesian(xlim = lim, ylim = lim)
}




#' Scatterplot with fitline for deconvolution results
#'
#' @param deconvolution deconvolution results in long format
#' @param actual actual fractions in long format
#'
#' @return A plot for each cell_type in the datasets
#' @export
#'
#' @examples
#' #' \dontrun{
#' plot_deconvolution_celltype_corrs(deconvultion, actual)
#'}

plot_deconvolution_celltype_corrs <- function(deconvolution, actual){
  estimate_v_actual <- dplyr::left_join(deconvolution, actual, by = dplyr::join_by(sample, cell_type),
                                 suffix = c("_estimate", "_actual"))

  cell_types <- unique(estimate_v_actual$cell_type)
  lapply(cell_types, plot_corr_celltype, est_v_actual = estimate_v_actual)
}
