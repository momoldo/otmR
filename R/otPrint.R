#' Print "otBasicStats" results
#'
#' @importFrom kableExtra kbl kable_classic
#' @param otm_obj an object computed by "otBasicStats"
#' @param ... further arguments passed to or from other methods.
#'
ot_print_basic_stats <- function(otm_obj, ...){
  dg <- ifelse(is.null(list(...)[["digits"]]),getOption("digits"),
               as.integer(list(...)[["digits"]]))
  tab_caption <- ifelse(is.null(list(...)[["caption"]]),"Basic Statistics",
                        list(...)[["caption"]])
  kbl(otm_obj, digits = dg, caption = tab_caption, align = "r") %>% kable_classic(full_width=FALSE)
}

#' Print "otCorrelation" results
#'
#' @importFrom kableExtra kbl kable_classic cell_spec
#' @param otm_obj an object computed by "otCorrelation"
#' @param ... further arguments passed to or from other methods.
#'
ot_print_colleration <- function(otm_obj, ...){
  dg <- ifelse(is.null(list(...)[["digits"]]),getOption("digits"),
               as.integer(list(...)[["digits"]]))
  tab_caption <- ifelse(is.null(list(...)[["caption"]]),"Correlation Matrix",
                        list(...)[["caption"]])
  is.colored <- ifelse(is.null(list(...)[["is.colored"]]), TRUE,
                       as.logical(list(...)[["is.colored"]]))
  if (is.colored){
    S <- format(round(otm_obj, dg), nsmall = dg)
    P <- attr(otm_obj, "otmR_P")
    for (i in 1:nrow(S)){
      for (j in 1:ncol(S)){
        if (P[i,j]<0.05){
          S[i,j] <- cell_spec(S[i,j], background = "lightgreen")
        }
      }
    }
    kbl(S, escape = FALSE, caption = tab_caption, align = "r") %>% kable_classic(full_width=FALSE)
  } else {
    kbl(otm_obj, digits = dg, caption = tab_caption, align = "r") %>% kable_classic(full_width=FALSE)
  }
}

#' Print "otGlm" results
#'
#' @importFrom kableExtra kbl kable_classic footnote
#' @param otm_obj an object computed by "otGlm"
#' @param ... further arguments passed to or from other methods.
#'
ot_print_glm <- function(otm_obj, ...){
  dg <- ifelse(is.null(list(...)[["digits"]]),getOption("digits"),
               as.integer(list(...)[["digits"]]))
  tab_caption <- ifelse(is.null(list(...)[["caption"]]),"Glm Result",
                        list(...)[["caption"]])
  fit <- attr(otm_obj, "otmR_fit")

  kbl(otm_obj, digits = dg, caption = tab_caption, align = "r") %>%
    kable_classic(full_width=FALSE) %>%
    footnote(general = paste0("R2=",format(round(fit$r.square,3), nsmall = 3),", F(",
                              fit$df_1,",",fit$df_2,")=",
                              format(round(fit$F.value,3), nsmall = 3),
                              ", p=",format(round(pf(fit$F.value,fit$df_1,fit$df_2, lower.tail = FALSE),3), nsmall = 3)),
             general_title = "Note:")
}

#' Print "otLogisticRegression" results
#'
#' @importFrom kableExtra kbl kable_classic footnote
#' @param otm_obj an object computed by "otLogisticRegression"
#' @param ... further arguments passed to or from other methods.
#'
ot_print_logistic_regression <- function(otm_obj, ...){
  dg <- ifelse(is.null(list(...)[["digits"]]),getOption("digits"),
               as.integer(list(...)[["digits"]]))
  tab_caption <- ifelse(is.null(list(...)[["caption"]]),"Logistic Regression Result",
                        list(...)[["caption"]])
  fit <- attr(otm_obj, "otmR_fit")

  kbl(otm_obj, digits = dg, caption = tab_caption, align = "r") %>%
    kable_classic(full_width=FALSE)
#    footnote(general = paste0("R2=",format(round(fit$r.square,3), nsmall = 3),", F(",
#                              fit$df_1,",",fit$df_2,")=",
#                              format(round(fit$F.value,3), nsmall = 3),
#                              ", p=",format(round(pf(fit$F.value,fit$df_1,fit$df_2, lower.tail = FALSE),3), nsmall = 3)),
#             general_title = "Note:")
}

#' Print out function for otmR
#'
#' @param otm_obj an object made by ot_functions.
#' @param ... further arguments passed to or from other methods.
#' @export
#'
otPrint <- function(otm_obj, ...){
  if (!is.null(otm_obj)){
    options(knitr.kable.NA = '')
    func_name <- attr(otm_obj, "otmR_func")
    if (!is.null(func_name)){
      switch (func_name,
              "BasicStats"  = ot_print_basic_stats(otm_obj, ...),
              "Correlation" = ot_print_colleration(otm_obj, ...),
              "Glm"         = ot_print_glm(otm_obj, ...),
              "LogisticRegression" = ot_print_logistic_regression(otm_obj, ...),
              print(otm_obj)
      )
    }
  }
}
