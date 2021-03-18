#' Cross Table
#'
#' otCrossTable is a function to make 2-dimensional cross table
#' from two variables.  It is similar to the function "table", but
#' otCrossTable use a formula to set two variables.  This is why
#' other ot-functions use formula style.
#'
#' @importFrom stats model.frame chisq.test fisher.test
#' @param data a data.frame object including two variables
#' @param model a formula "var_a ~ var_b" style
#' @param omit.na a logical value.  If TRUE, NA is deleted
#'   from a cross table.
#' @param is.test a logical value.  If TRUE, chisq.test and
#'   fisher.test are submitted.
#' @export

otCrossTable <- function(data, model, omit.na=FALSE, is.test=TRUE){
  if (!is.null(model)){
    na_action <- ifelse(omit.na, "no","ifany")
    d <- model.frame(model, data = data)
    tbl <- table(d[,1],d[,2], useNA = na_action)

    res <- data.frame(tbl)
    names(res) <- c(names(d[,1:2]),"f")
    attr(res, "otmR_func")  <- "CrossTable"
    attr(res, "otmR_model") <- model

    if (is.test){
      res.chi <- chisq.test(tbl,  correct = FALSE)
      res.fis <- fisher.test(tbl, simulate.p.value = TRUE)

      Test <- c("Pearson's Chi-squared Test", "Fisher's Exact Test")
      Chi_squared <- c(res.chi$statistic, NA)
      df <- c(res.chi$parameter, NA)
      p.value <- c(res.chi$p.value, res.fis$p.value)
      attr(res, "otmR_test") <- data.frame(Test, Chi_squared, df, p.value, row.names = NULL)
    }

    return(res)
  }
}
