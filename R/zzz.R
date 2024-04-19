.onLoad <- function(libname, pkgname){
  reticulate::use_condaenv("cellanneal")
  reticulate::source_python(
    system.file("python/run_cellanneal.py", package = "deconfectHelpers"),
    envir = deconv_env
  )
  reticulate::source_python(
    system.file("python/toy.py", package = "deconfectHelpers"),
    envir = deconv_env
  )
}
