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
    df_label <- v_label[!is.na(v_label)] # remove NA
    df_label <-
      strsplit(gsub("[[:blank:]]","",df_label), split = "=") %>% # remove White Space
      purrr::map_dfr(~data.frame(lv=.x[1],lb=.x[2]))

    if (sum(is.na(df_label[,2]))>0){ # not "level=label" format
      res <- factor(x, labels = df_label[,1])
    } else { # "level=label" format
      res <- factor(x, levels = df_label[,1], labels = df_label[,2])
    }
    return(res)
  } else {
    return(x)
  }
}
