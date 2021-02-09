#' Anova Tables
#'
#' otAnova is a funtion to compute ANOVA tables.
#'
#' @importFrom stats anova aov
#' @param data a data.frame to compute ANOVA tables.
#' @param model a formula to compute ANOVA tables.
#' @export
#'
otAnova <- function(data, model=NULL){
  if (!is.null(model)){
    res <- anova(aov(model, data = data))

    attr(res, "otmR_function") <- "ANOVA"
    attr(res, "otmR_model") <- model

    return(res)
  } else {
    return(NULL)
  }
}
