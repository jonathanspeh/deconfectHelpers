#' Create conda environment
#'
#' @return A conda environment
#' @export
#'
install_cellanneal <- function(){
  reticulate::conda_create("cellanneal-r", "python==3.8")
  reticulate::conda_install("cellanneal-r", c("numpy==1.24", "scipy==1.9",
                                              "matplotlib==3.7", "pandas==1.5",
                                              "seaborn==0.12"))
  reticulate::conda_install("cellanneal-r",
                            c("openpyxl==3.0",
                              "xlrd==1.2"),
                            channel = "anaconda")



  }

