.onLoad <- function(libname, pkgname){
  tryCatch(reticulate::use_condaenv("cellanneal-r"),
           error = function(e){
             #packageStartupMessage("Creating cellanneal-r conda environment and installing dependencies")
             reticulate::conda_create("cellanneal-r", "python==3.8")
             reticulate::conda_install("cellanneal-r", c("numpy==1.24", "scipy==1.9",
                                                         "matplotlib==3.7", "pandas==1.5",
                                                         "seaborn==0.12"))
             reticulate::conda_install("cellanneal-r",
                                       c("openpyxl==3.0",
                                         "xlrd==1.2"),
                                       channel = "anaconda")
             reticulate::use_condaenv("cellanneal-r")
             curl::curl_download("https://github.com/LiBuchauer/cellanneal/archive/refs/heads/master.zip", destfile = "./cellanneal.zip")
             unzip("cellanneal.zip")
             reticulate::py_install("cellanneal-master/", pip = TRUE)
             file.remove("cellanneal.zip")
             unlink("cellanneal-master", recursive = TRUE)
           })
  reticulate::source_python(
    system.file("python/run_cellanneal.py", package = "deconfectHelpers"),
    envir = deconv_env
  )
}




# tryCatch(reticulate::use_condaenv("cellanneal"),
#          error = function(e){
#            print("condaenv doesn't exist")
#          }
#          )
