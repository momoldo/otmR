#' Student's t-Test
#'
#' otTTest is a function of statistical test of means for two sample.
#'
#' @importFrom stats t.test var.test
#' @param data a data.frame object including including both a
#'   dependent variables and independent
#'   variables.
#' @param model a formula of the same form lhs ~ rhs as stats::t.test
#' @param p.value a probability of significant level
#' @export

otTTest <- function(data, model, p.value = 0.05){
  if (!is.null(model)){
    res.var.test <- var.test(model, data = data, conf.level = 1-p.value)
    res.t.test.equal  <- t.test(model, data = data, var.equal = TRUE,
                                conf.level = 1-p.value)
    res.t.test.noteq <-  t.test(model, data = data, var.equal = FALSE,
                                conf.level = 1-p.value)
    F.value <- c(res.var.test$statistic, NA)
    F.df_1  <- c(res.var.test$parameter[1],NA)
    F.df_2  <- c(res.var.test$parameter[2],NA)
    F.p.value <- c(res.var.test$p.value,NA)
    t.value <- c(res.t.test.equal$statistic,
                 res.t.test.noteq$statistic)
    t.df    <- c(res.t.test.equal$parameter,
                 res.t.test.noteq$parameter)
    t.p.value <- c(res.t.test.equal$p.value,
                   res.t.test.noteq$p.value)
    conf.low  <- c(res.t.test.equal$conf.int[1],
                   res.t.test.noteq$conf.int[1])
    conf.high <- c(res.t.test.equal$conf.int[2],
                   res.t.test.noteq$conf.int[1])
    res <- data.frame(F.value, F.df_1, F.df_2, F.p.value,
                      t.value, t.df, t.p.value, conf.low, conf.high,
                      row.names = c("Var is equal", "Var is not equal"))
    attr(res, "otmR_func") <- "TTest"
    attr(res, "otmR_model") <- model
    attr(res, "otmR_alpha") <- p.value

    return(res)
  }
}
