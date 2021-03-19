#' Fitting Generalized Linear Models for otmR
#'
#' otGlm is used to fit generalized linear models.  otGlm returns
#' a data.frame including simple results of stats::glm().
#'
#' @importFrom purrr modify
#' @importFrom dplyr select
#' @importFrom tibble add_column
#' @importFrom stats model.frame model.matrix.lm model.response
#'     glm glm.fit gaussian summary.lm pf formula
#' @param data a data.frame object including both a dependent variables and independent
#'   variables.
#' @param model an object of class "formula": a symbolic description
#'   of the model to be fitted.
#' @param is.residual a logical value indicating whether return object
#'   have residual information or not.
#' @export
#'
otGlm <- function(data, model=NULL, is.residual=FALSE){
  if ((!is.null(data))&&(ncol(data)>=2)){
    if (is.null(model)){ # if NULL, model formula is made from data[,1]~data[,2]+data[,3]...
      model <- formula(paste0(names(data)[1],"~", paste0(names(data)[-1],collapse = "+")))
    }
    d <- model.frame(model, data = data)
    res.fit <- glm(model, data = data.frame(d), family = gaussian)
    v.dep <- model.matrix.lm(d) %>% data.frame() %>%
      select(-1) %>% # remove intercept
      modify(scale) %>% # standardize
      add_column(Intercept=1, .before = 1) # append intercept
    res.fit.std <- glm.fit(x = v.dep, y = scale(model.response(d)), family = gaussian())
    res.summary <- summary(res.fit)
    res <- data.frame(non.std.b = res.fit$coefficients,
                      std.b     = res.fit.std$coefficients,
                      std.err   = res.summary$coefficients[,2],
                      t.value   = res.summary$coefficients[,3],
                      df        = res.summary$df.residual,
                      p.value   = res.summary$coefficients[,4])
    attr(res, "otmR_func") <- "Glm"

    res.summary.lm <- summary.lm(res.fit)

    attr(res, "otmR_model") <- res.fit$formula
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
      rsd <- data.frame(
        ID = c(1:length(res.fit$y)),
        Y = res.fit$y, Y_Hat = res.fit$fitted.values,
        Residual     = res.fit$residuals,
        Std.residual = res.fit.std$residuals, row.names = NULL)
      rsd <- rsd[order(rsd$Std.residual, decreasing = TRUE),]

      attr(res, "otmR_residual") <- rsd
    }
    return(res)
  }
}
