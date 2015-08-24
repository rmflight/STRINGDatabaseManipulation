#' process raw STRING data
#'
#' given a STRING data file, process it and save it for future use
#'
#' @param string_file the data file
#' @export
#' @return data.frame
#'
#' @example
#'
process_string_data <- function(string_file){
  stopifnot(file.exists(string_file))

  string_data <- read.table(string_file, header = TRUE, sep = " ", stringsAsFactors = FALSE)
  return(string_data)
}

#' process STRING ID files
#'
#' given a STRING ID file, generate a data.frame that can be used to map
#' various symbols
#'
#' @param string_file the id file
#' @export
#' @return data.frame
#'
#' @example
#'
process_string_id <- function(string_file){
  stopifnot(file.exists(string_file))

  string_id <- read.table(string_file, sep = "\t", header = FALSE, stringsAsFactors = FALSE, quote = "", fill = TRUE)
  names(string_id) <- c("string", "other", "type")
  return(string_id)
}
