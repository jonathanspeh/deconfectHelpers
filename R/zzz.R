# .onAttach <- function(libname, pkgname){
#               if(!reticulate::condaenv_exists("deconvolute-r")){
#                 packageStartupMessage("Creating deconvolute-r conda environment and installing cellanneal and its dependencies")
#                 reticulate::conda_create("deconvolute-r", "python==3.10", channel = "anaconda")
#                 reticulate::conda_install("deconvolute-r", c("numpy==1.24", "scipy==1.9",
#                                                              "matplotlib==3.7", "pandas==1.5",
#                                                              "seaborn==0.12"))
#                 reticulate::conda_install("deconvolute-r",
#                                            c("openpyxl==3.0",
#                                              "xlrd==1.2"),
#                                            channel = "anaconda")
#                 reticulate::use_condaenv("deconvolute-r")
#                 curl::curl_download("https://github.com/LiBuchauer/cellanneal/archive/refs/heads/master.zip", destfile = "./cellanneal.zip")
#                 unzip("cellanneal.zip")
#                 reticulate::py_install("cellanneal-master/", pip = TRUE)
#                 file.remove("cellanneal.zip")
#                 unlink("cellanneal-master", recursive = TRUE)
#               }
#             reticulate::use_condaenv(condaenv = "deconvolute-r")
#             reticulate::source_python(
#               system.file("python/run_cellanneal.py",
#                           package = "deconfectHelpers"),
#               envir = deconv_env)}
#
