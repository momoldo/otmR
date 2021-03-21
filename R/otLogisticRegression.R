#' Logistic Regression
#'
#' otLogisticRegression is used to fit logistic models,
#' using glm functions.  An independent variable must have
#' 0-1 value.
#'
#' @importFrom purrr modify
#' @importFrom stats model.frame glm binomial residuals formula
#' @param data a data.frame object including both a dependent variables and independent
#'   variables.
#' @param model an object of class "formula": a symbolic description
#'   of the model to be fitted.
#' @param is.residual a logical value indicating whether return object
#'   have residual information or not.
#' @export
#'
otLogisticRegression <- function(data, model=NULL, is.residual=FALSE){
  if ((!is.null(data))&&(ncol(data)>=2)){
    if (is.null(model)){ # if NULL, model formula is made from data[,1]~data[,2]+data[,3]...
      model <- formula(paste0(names(data)[1],"~", paste0(names(data)[-1],collapse = "+")))
    }
    d <- data.frame(model.frame(model, data = data))

    res.fit <- glm(model, data = d, family = binomial(link = "logit"))
    res.summary <- summary(res.fit)
    res <- data.frame(beta = res.fit$coefficients,
                      odds.ratio = exp(c(NA, res.fit$coefficients[2:length(res.fit$coefficients)])),
                      std.err    = res.summary$coefficients[,2],
                      z.score    = res.summary$coefficients[,3],
                      p.value    = res.summary$coefficients[,4])
    attr(res, "otmR_func") <- "LogisticRegression"

    attr(res, "otmR_model") <- res.fit$formula

    pred <- data.frame(
      Y = res.fit$y,
      Y.HAT = round(res.fit$fitted.values,0),
      Y.pred = res.fit$fitted.values,
      devience = residuals(res.fit), row.names = NULL)

    confusion_matrix <- table(pred$Y.HAT, pred$Y)
    if ((nrow(confusion_matrix)==2)&&(ncol(confusion_matrix)==2)){
      TP <- confusion_matrix[2,2] # Y=1, Y.HAT=1, True_Positive
      TN <- confusion_matrix[1,1] # Y=0, Y.HAT=0, True_Negative
      FP <- confusion_matrix[1,2] # Y=1, Y.HAT=0, False_Positive
      FN <- confusion_matrix[2,1] # Y=0, Y.HAT=1, False_Negative
      fit <- data.frame(
        T_Pos = TP, T_Neg = TN, F_Pos = FP, F_Neg = FN,
        r.accuracy  = (TP + TN) / (TP + TN + FP + FN),
        r.precision = TP / (TP + FP),
        r.recall = TP / (TP + FN),
        r.specificity = TN / (FP + TN)
      )
    } else {
      fit <- data.frame(
        T_Pos = NA, T_Neg = NA, F_Pos = NA, F_Neg = NA,
        r.accuracy  = NA,
        r.precision = NA,
        r.recall = NA,
        r.specificity = NA
      )
    }

    attr(res, "otmR_fit") <- fit
    if (is.residual){
      attr(res, "otmR_residual") <- pred[order(abs(pred$devience),decreasing = TRUE),]
    }
    return(res)
  }
}
