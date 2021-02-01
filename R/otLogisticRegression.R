#' Logistic Regression
#'
#' otLogisticRegression is used to fit logistic models,
#' using glm functions.  An independent variable must have
#' 0-1 value.
#'
#' @importFrom purrr modify
#' @importFrom stats model.frame glm binomial residuals
#' @param data a data.frame object including both a dependent variables and independent
#'   variables.
#' @param model an object of class "formula": a symbolic description
#'   of the model to be fitted.
#' @param is.residual a logical value indicating whether return object
#'   have residual information or not.
#' @export
#'
otLogisticRegression <- function(data, model=NULL, is.residual=FALSE){
  if (!is.null(model)){
    d <- data.frame(model.frame(model, data = data))
    res.fit <- glm(model, data = d, family = binomial(link = "logit"))
    res.summary <- summary(res.fit)
    res <- data.frame(beta = res.fit$coefficients,
                      odds.ratio = exp(c(NA, res.fit$coefficients[2:length(res.fit$coefficients)])),
                      std.err    = res.summary$coefficients[,2],
                      z.score    = res.summary$coefficients[,3],
                      p.value    = res.summary$coefficients[,4])
    attr(res, "otmR_func") <- "LogisticRegression"

    attr(res, "otmR_residual") <- data.frame(
      Y = res.fit$y, Y.HAT = res.fit$fitted.values,
      devience = residuals(res.fit), row.names = NULL)

    return(res)
  }
}
