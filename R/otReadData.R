#' otReadData
#'
#' otReadData is a function to make a data.frame from a xlsx book file.
#' If a sheet2 on a xlsx book file includes descriptions of each variables,
#' this function mutates a numeric vector to a factor one.
#'
#' @importFrom readxl read_excel
#' @param path path to the xls/xlsx file.
#' @param sheet sheet to read.  Ether a string (the name of sheet),
#'   or an integer (the position of the sheet).
#' @param ... further arguments passed to or from other methods.
#' @export
otReadData <- function(path, sheet = NULL, ...){ # not yet...
  data <- read_excel(path, sheet, ...)
}
