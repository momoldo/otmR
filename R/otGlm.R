#' Fitting Generalized Linear Models for otmR
#'
#' otGlm is used to fit generalized linear models.  otGlm returns
#' a data.frame including simple results of stats::glm().
#'
#' @importFrom purrr modify
#' @param data a data.frame object including both a dependent variables and independent
#'   variables.
#' @param model an object of class "formula": a symbolic description
#'   of the model to be fitted.
#' @export
#'
otGlm <- function(data, model=NULL, is.residual=FALSE){
  if (!is.null(model)){
    d <- data.frame(model.frame(model, data = data))
    res.fit <- glm(model, data = d, family = gaussian)
    d <- d %>% purrr::modify(scale)
    res.fit.std <- glm(model, data = d, family = gaussian)
    res.summary <- summary(res.fit)
    res <- data.frame(non.std.b = res.fit$coefficients,
                      std.b     = res.fit.std$coefficients,
                      std.err   = res.summary$coefficients[,2],
                      t.value   = res.summary$coefficients[,3],
                      d.f       = res.summary$df.residual,
                      p.value   = res.summary$coefficients[,4])
    attr(res, "otmR_func") <- "Glm"

    res.summary.lm <- summary.lm(res.fit)
    attr(res, "otmR_fit") <- data.frame(
    N = length(res.fit$y),            # sample size
    P = length(res.fit$coefficients), # number of independent variables
    r.square     = res.summary.lm$r.squared,
    adj.r.square = res.summary.lm$adj.r.squared,
    aic          = res.summary$aic,
    F.value      = res.summary.lm$fstatistic[["value"]],
    df_1 = res.summary.lm$fstatistic[["numdf"]],
    df_2 = res.summary.lm$fstatistic[["dendf"]],
    p.value = pf(res.summary.lm$fstatistic[["value"]],
                 res.summary.lm$fstatistic[["numdf"]],
                 res.summary.lm$fstatistic[["dendf"]], lower.tail = FALSE))

    if (is.residual){
      attr(res, "otmR_residual") <- data.frame(
        Y = res.fit$y, Y_Hat = res.fit$fitted.values,
        Residual     = res.fit$residuals,
        Std_Residual = res.fit.std$residuals, row.names = NULL)
    }
    return(res)
  }
}
