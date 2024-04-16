# Find suspicious names -----
# Checks for gene names that look like excel converted them into dates

find_suspicious_names <- function(dat, gene_names, index = TRUE){
  stopifnot("gene_names must be a valid column name" = gene_names %in% colnames(dat),
            "index must be logical" = is.logical(index))
  if(index){
    dat <- tibble::rowid_to_column(dat, "row_index")
  }
  suppressWarnings({
    susp_names <- dat[!is.na(as.numeric(dat[[gene_names]])),]
  })
  if(nrow(susp_names) == 0) message("no suspicious gene names found")
  else susp_names
}
