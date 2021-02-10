#' otReadData
#'
#' otReadData is a function to make a data.frame from a xlsx book file.
#' If a sheet2 on a xlsx book file includes descriptions of each variables,
#' this function mutates a numeric vector to a factor one.
#'
#' @importFrom readxl read_excel excel_sheets
#' @param path path to the xls/xlsx file.
#' @param sheet sheet to read data.  Ether a string (the name of sheet),
#'   or an integer (the position of the sheet).
#' @param label_file path to the xls/xlsx file to read value labels.
#' @param label_sheet sheet to read value labels. Ether a string or an integer.
#' @param ... further arguments passed to or from other methods.
#' @export
#'
otReadData <- function(path, sheet = NULL,
                       label_file = path, label_sheet = "Label", ...){
  if (file.exists(path)){
    data   <- read_excel(path, sheet = sheet, ...)

    if(file.exists(label_file)){
      sheets <- excel_sheets(path)
      if (!is.na(match(label_sheet, sheets))){
        lab  <- read_excel(label_file, sheet = label_sheet)
        for (i in 1:nrow(lab)){
          var_name <- as.character(lab[i,1])
          data[,var_name] <- ot_set_variable(data[[var_name]], v_label = lab[i,-1])
        }
      }
    }
    return(data)
  } else {
    return(NULL)
  }
}
