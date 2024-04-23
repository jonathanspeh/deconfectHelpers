#' Plot Violin
#'
#' @param deconvolution deconvolution results in long format
#' @param actual actual fractions in long format
#'
#' @return A ggplot2 object
#' @importFrom ggplot2 .data
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
    dplyr::mutate(group = paste(.data$cell_type, .data$flag, sep = "_")) |>
      ggplot2::ggplot(ggplot2::aes(x = .data$cell_type, y = .data$proportion, fill = .data$flag)) +
      ggplot2::geom_violin() +
      ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(n.dodge = 2))
  }




#' Internal plot function
#'
#' @param data a data frame containing estimated and actual fractions
#' @param cell_type_filter a vector of celltypes to plot
#' @param add_x_y_line Boolean to specify wether to plot red x == y line
#'
#' @return a plot of the selected celltype
#' @importFrom ggplot2 .data
#'
#' @examples
#' \dontrun{
#' plot_corr_celltype(data, cell_type_filter)
#' }


# TODO: add x==y line (maybe as optional)
plot_corr_celltype <- function(data, cell_type_filter, add_x_y_line = add_x_y_line){

  subs <- dplyr::filter(data, .data$cell_type == cell_type_filter)

  min <- min(min(subs["proportion_estimate"]),min(subs["proportion_actual"]))
  max <- max(max(subs["proportion_estimate"]),max(subs["proportion_actual"]))
  lim <- c(min, max)

  p <- subs |>
    ggplot2::ggplot(ggplot2::aes(x = .data$proportion_actual, y = .data$proportion_estimate)) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method = "lm") +
    ggplot2::ggtitle(cell_type_filter) +
    ggplot2::coord_cartesian(xlim = lim, ylim = lim)

  if(add_x_y_line){
    p +
      ggplot2::geom_abline(slope = 1,
                  colour = "red")
  } else {
      p
    }
 }



#' Scatterplot with fitline for deconvolution results
#'
#' @param deconvolution deconvolution results in long format
#' @param actual actual fractions in long format
#' @param add_x_y_line Boolean to specify wether to plot red x == y line
#'
#' @return A plot for each cell_type in the datasets
#' @export
#'
#' @examples
#' \dontrun{
#' plot_deconvolution_celltype_corrs(deconvolution, actual)
#'}

plot_deconvolution_celltype_corrs <- function(deconvolution, actual, add_x_y_line = TRUE){

  estimate_v_actual <- dplyr::left_join(deconvolution, actual, by = dplyr::join_by("sample", "cell_type"),
                                 suffix = c("_estimate", "_actual"))

  cell_types <- unlist(unique(estimate_v_actual["cell_type"]))
  lapply(cell_types, plot_corr_celltype, data = estimate_v_actual, add_x_y_line = add_x_y_line)

}


#' Show corrolation between deconvolution results and actual fractions
#'
#' @param deconvolution deconvolution results in long format
#' @param actual actual fractions in long format
#' @param add_x_y_line Boolean to specify wether to plot red x == y line
#' @param add_metrics Boolean to specify wether to add RMSE and pearsons corrolation
#'
#' @return a gggplot2 object
#' @export
#'
#' @examples
#' \dontrun{
#' plot_deconvolution_corrs(deconvolution, actual)
#'}

plot_deconvolution_corrs <- function(deconvolution,
                                     actual,
                                     add_x_y_line = TRUE,
                                     add_metrics = TRUE) {
  p <- dplyr::left_join(deconvolution, actual, by = dplyr::join_by("sample", "cell_type"),
                   suffix = c("_estimated", "_actual")) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$proportion_actual, .data$proportion_estimated)) +
    ggplot2::geom_point(ggplot2::aes(colour = .data$cell_type)) +
    ggplot2::geom_smooth(method = "lm")

  if(add_x_y_line) {
    p <- p +
      ggplot2::geom_abline(slope = 1,
                           colour = "red")

  }
  if(add_metrics){
    rmse <- get_rmse(deconvolution, actual)
    cors <- stats::cor.test(deconvolution$proportion, actual$proportion)
    p <- p +
      ggplot2::annotate("text", label = paste0("RMSE = ", round(rmse, 4), "\nR = ", round(cors$estimate, 2), "\nP =  ", round(cors$p.value, 4)),
               x = min(deconvolution$proportion),
               y = max(actual$proportion),
               hjust = 0,
               vjust = 0.7)

  }
  p
}





