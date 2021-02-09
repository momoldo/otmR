#' otReadData
#'
#' otReadData is a function to make a data.frame from a xlsx book file.
#' If a sheet2 on a xlsx book file includes descriptions of each variables,
#' this function mutates a numeric vector to a factor one.
#'
#' @importFrom readxl read_excel
#' @param path path to the xls/xlsx file.
#' @param sheet sheet to read data.  Ether a string (the name of sheet),
#'   or an integer (the position of the sheet).
#' @param label_sheet sheet to read value label. Ether a string or an integer.
#' @param ... further arguments passed to or from other methods.
#' @export
otReadData <- function(path, sheet = NULL, label_sheet = "Label", ...){ # not yet...
  if (file.exists(path)){
    data   <- read_excel(path, sheet = sheet, ...)
    sheets <- read_excel(path)
    lab <- NULL

    if (!is.na(match(label_sheet, sheets))){
      lab  <- read_excel(path, sheet = label_sheet)
    }

    if (!is.null(lab)){
      lab <- gsub("[[:blank:]]","",lab) # delete white spaces and tab
    }
    return(data)
  } else {
    return(NULL)
  }
}
