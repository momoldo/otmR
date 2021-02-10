#' Setting value labels to a factor variable
#'
#' ot_set_variable is the function to put value labels like SPSS
#' to a integer variable.  It returns a factor variable.
#'
#' @importFrom purrr map_dfr
#' @param x a vector to set value labels.
#' @param v_label a label string vector: the format "level=label".
#'
ot_set_variable <- function(x, v_label=NULL){
  if (!is.null(v_label)){
    df_label <- strsplit(gsub("[[:blank:]]","",v_label), split = "=") %>%
      purrr::map_dfr(~data.frame(lv=.x[1],lb=.x[2]))
    res <- factor(x, levels = df_label[,1], labels = df_label[,2])
    return(res)
  } else {
    return(x)
  }
}
