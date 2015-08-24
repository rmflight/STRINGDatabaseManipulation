#' strip species id from identifier
#'
#' given a set of STRING ID's, remove the species taxonomy id part to get an ENSEMBL
#' protein ID.
#'
#' @param string_id character vector of STRING IDs (XXXX.ENSPXXX)
#'
#' @return character vector
#'
strip_species <- function(string_id){
  substring(string_id, 6)
}
