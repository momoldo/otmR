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
#' @importFrom kableExtra kbl kable_classic column_spec
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
  alpha      <- ifelse(is.null(list(...)[["alpha"]]), 0.05,
                       as.numeric(list(...)[["alpha"]]))
  hi.correlation <- ifelse(is.null(list(...)[["hi.correlation"]]), 0.35,
                       as.numeric(list(...)[["hi.correlation"]]))

  res <- otm_obj %>% kbl(digits = dg, caption = tab_caption, align = "r") %>%
    kable_classic(full_width=FALSE)

  if (is.colored){
    P <- attr(otm_obj, "otmR_P")
    for (i in 1:nrow(otm_obj)){ # res assumes [x,1+x] because otm_obj contains row.names
      res <- column_spec(res, i+1,
                         color = ifelse(otm_obj[,i]<hi.correlation,"#ff99cc","black"),
                         background = ifelse(P[,i]>alpha, "white","#99ffcc"))
    }
  }

  return(res)
}

#' Print "otTTest" results
#'
#' @importFrom kableExtra kbl kable_classic column_spec footnote
#' @param otm_obj an object computed by "otTTest"
#' @param ... further arguments passed to or from other methods.
#'
ot_print_ttest <- function(otm_obj, ...){
  dg    <- ifelse(is.null(list(...)[["digits"]]),getOption("digits"),
                  as.integer(list(...)[["digits"]]))
  alpha <- ifelse(is.null(attr(otm_obj, "otmR_alpha")), 0.95,
                  as.numeric(attr(otm_obj, "otmR_alpha")))
  c.model <- as.character(attr(otm_obj, "otmR_model"))
  tab_caption <- ifelse(is.null(list(...)[["caption"]]),
                        paste0("Student's t-Test Result(",c.model[2],"~",c.model[3],")"),
                        list(...)[["caption"]])
  is.colored <- ifelse(is.null(list(...)[["is.colored"]]), TRUE,
                       as.logical(list(...)[["is.colored"]]))

  if (!is.null(otm_obj)){
    tbl <- format(round(otm_obj, dg), nsmall = dg)
    tbl[1,6] <- sub("\\.0*","", tbl[1,6]) # adopt integer type to var.equal's t.df
    tbl[2,] <- sub("NA","", tbl[2,])      # delete character NA

    tbl <- kbl(tbl, escape = FALSE, caption = tab_caption, align = "r") %>%
        kable_classic(full_width=FALSE) %>%
        footnote(general = paste0("alpha is ",alpha), general_title = "Note:")

    if (is.colored){
      tbl <- column_spec(tbl,6:8, color = ifelse(otm_obj[,7]<alpha,"red","black"))
        # otm_obj contains row.name, so otm_obj is [2,9] but tbl assumes [2,1+9]
      tbl <- row_spec(tbl, ifelse(otm_obj[1,4]>alpha,1,2), background = "#99ffcc")
    }

    return(tbl)
  }
}

#' Print "otCrossTable" results
#'
#' @importFrom kableExtra column_spec
#' @importFrom tidyr pivot_wider
#' @param otm_obj an object computed by "otCrossTable"
#' @param ... further arguments passed to or from other methods.
ot_print_crosstable <- function(otm_obj, ...){
  dg <- ifelse(is.null(list(...)[["digits"]]),getOption("digits"),
               as.integer(list(...)[["digits"]]))
  c.model <- as.character(attr(otm_obj, "otmR_model"))
  tab_caption <- ifelse(is.null(list(...)[["caption"]]),
                        paste0("Contingency Table(",c.model[2],"~",c.model[3],")"),
                        list(...)[["caption"]])
  is.colored <- ifelse(is.null(list(...)[["is.colored"]]), TRUE,
                       as.logical(list(...)[["is.colored"]]))

  cross.tbl <- otm_obj %>% pivot_wider(names_from = 2, values_from = 3)
  row_name <- as.character(cross.tbl[,1])

  return.cross.tbl <-
    cross.tbl %>% kbl(caption = tab_caption, align = "r") %>%
    kable_classic(full_width=FALSE) %>%
    column_spec(1, border_right = TRUE, bold = TRUE)

  if (is.colored){
    for (i in 2:ncol(cross.tbl)){
      return.cross.tbl <- column_spec(return.cross.tbl, i,
                                      color = ifelse(cross.tbl[,i]<5,"red","black"))
    }
  }

  if (!is.null(attr(otm_obj, "otmR_test"))){
    res.test <- attr(otm_obj, "otmR_test")
    tbl <- format(round(res.test[,-1], dg), nsmall = dg)
    tbl[2,1:2] <- sub("NA","", tbl[2,1:2])  # delete character NA
    tbl[1,2]   <- sub("\\.0*", "", tbl[1,2]) # delete .0000 from df
    return.test <-
      data.frame(Test=res.test$Test, tbl) %>%
      kbl(escape = FALSE, caption = "Result of Test", align = "r") %>%
      kable_classic(full_width = FALSE) %>%
      column_spec(1,border_right = TRUE)

    return(list(return.cross.tbl, return.test))
  } else {
    return(return.cross.tbl)
  }
}

#' Print "otGlm" results
#'
#' @importFrom kableExtra kbl kable_classic footnote
#' @importFrom utils head
#' @param otm_obj an object computed by "otGlm"
#' @param ... further arguments passed to or from other methods.
#'
ot_print_glm <- function(otm_obj, ...){
  dg <- ifelse(is.null(list(...)[["digits"]]),getOption("digits"),
               as.integer(list(...)[["digits"]]))
  c.model <- as.character(attr(otm_obj, "otmR_model"))
  tab_caption <- ifelse(is.null(list(...)[["caption"]]),
                        paste0("Glm Result(",c.model[2],"~",c.model[3],")"),
                        list(...)[["caption"]])
  fit <- attr(otm_obj, "otmR_fit")
  is.colored <- ifelse(is.null(list(...)[["is.colored"]]), TRUE,
                       as.logical(list(...)[["is.colored"]]))

  if (is.colored){
    color_row <- which(otm_obj$p.value < 0.05) # 0.05 is replaced by option in the future
    tab <- kbl(otm_obj, digits = dg, caption = tab_caption, align = "r") %>%
      row_spec(color_row, background = "lightgreen")

  } else {
    tab <- kbl(otm_obj, digits = dg, caption = tab_caption, align = "r")
  }

  return.tab <-
    tab %>%
      kable_classic(full_width=FALSE) %>%
      footnote(general = paste0("R2=",format(round(fit$r.square,3), nsmall = 3),", F(",
                                fit$df_1,",",fit$df_2,")=",
                                format(round(fit$F.value,3), nsmall = 3),
                                ", p=",format(round(pf(fit$F.value,fit$df_1,fit$df_2, lower.tail = FALSE),3),
                                              nsmall = 3)),
               general_title = "Note:")

  df.res <- attr(otm_obj, "otmR_residual")
  if (!is.null(df.res)){
    return.residual <-
      df.res %>% head() %>%
        kbl(digits = dg, caption = "Rank of Residuals", align = "r") %>%
        kable_classic(full_width=FALSE)
    return(list(return.tab, return.residual))
  } else {
    return(return.tab)
  }
}

#' Print "otLogisticRegression" results
#'
#' @importFrom kableExtra kbl kable_classic footnote row_spec
#' @param otm_obj an object computed by "otLogisticRegression"
#' @param ... further arguments passed to or from other methods.
#'
ot_print_logistic_regression <- function(otm_obj, ...){
  dg <- ifelse(is.null(list(...)[["digits"]]),getOption("digits"),
               as.integer(list(...)[["digits"]]))
  c.model <- as.character(attr(otm_obj, "otmR_model"))
  tab_caption <- ifelse(is.null(list(...)[["caption"]]),
                        paste0("Glm Result(",c.model[2],"~",c.model[3],")"),
                        list(...)[["caption"]])
  fit <- attr(otm_obj, "otmR_fit")
  is.colored <- ifelse(is.null(list(...)[["is.colored"]]), TRUE,
                       as.logical(list(...)[["is.colored"]]))

  if (is.colored){
    color_row <- which(otm_obj$p.value < 0.05) # 0.05 is replaced by option in the future
    tab <- kbl(otm_obj, digits = dg, caption = tab_caption, align = "r") %>%
      row_spec(color_row, background = "lightgreen")

  } else {
    tab <- kbl(otm_obj, digits = dg, caption = tab_caption, align = "r")
  }

  fit <- attr(otm_obj, "otmR_fit")

  return.tab <-
    tab %>% kable_classic(full_width=FALSE) %>%
      footnote(general = paste0("Kappa=", format(round(fit$Kappa,3), nsmall = 3),
                                ", AIC=", format(round(fit$AIC,3), nsmall = 3),
                                ", BIC=", format(round(fit$BIC,3), nsmall = 3), "\n",
                                "Accuracy=",    format(round(fit$r.accuracy,3), nsmall = 3),
                                ", Precision=",   format(round(fit$r.precision,3), nsmall = 3),
                                ", Recall=",      format(round(fit$r.recall,3), nsmall = 3),
                                ", Specificity=", format(round(fit$r.specificity,3), nsmall = 3), "\n",
                                "TP=", fit$T_Pos, ", TN=", fit$T_Neg,
                                ", FP=", fit$F_Pos, ", FN=", fit$F_Neg, "\n",
                                "Acr=(TP+TN)/TTL, Prc=TP/(TP+FP), Rcl=TP/(TP+FN), Spc=TN/(FP+TN)"),
               general_title = "Note:")

  df.res <- attr(otm_obj, "otmR_residual")
  if (!is.null(df.res)){
    return.residual <-
      df.res %>% head() %>%
        kbl(digits = dg, caption = "Rank of Residuals", align = "r") %>%
        kable_classic(full_width=FALSE)
    return(list(return.tab, return.residual))
  } else {
    return(return.tab)
  }
}

#' Print "otANOVA" resutls
#'
#' otANOVA is a function to print out "otANOVA" results.
#'
#' @importFrom kableExtra kbl kable_classic row_spec
#' @param otm_obj an object make by ot_functions.
#' @param ... further arguments passed to or from other methods.
#'
ot_print_anova <- function(otm_obj, ...){
  dg <- ifelse(is.null(list(...)[["digits"]]),getOption("digits"),
               as.integer(list(...)[["digits"]]))
  c.model <- as.character(attr(otm_obj, "otmR_model"))
  tab_caption <- ifelse(is.null(list(...)[["caption"]]),
                        paste0("ANOVA Tables(",c.model[2],"~",c.model[3],")"),
                        list(...)[["caption"]])
  is.colored <- ifelse(is.null(list(...)[["is.colored"]]), TRUE,
                       as.logical(list(...)[["is.colored"]]))

  if (is.colored){
    color_row <- which(otm_obj[,5] < 0.05) # 0.05 is replaced by option in the future
    tab <- kbl(otm_obj, digits = dg, caption = tab_caption, align = "r") %>%
      row_spec(color_row, background = "lightgreen")
  } else {
    tab <- kbl(otm_obj, digits = dg, caption = tab_caption, align = "r")
  }
  tab %>%
    kable_classic(full_width=FALSE)
}

#' Print out function for otmR
#'
#' @param otm_obj an object made by ot_functions.
#' @param digits a numeric how many significant digits are to be used for
#'   numeric and comlex value.
#' @param ... further arguments passed to or from other methods.
#' @export
#'
otPrint <- function(otm_obj, digits = 3, ...){
  if (!is.null(otm_obj)){
    options(knitr.kable.NA = '')
    func_name <- attr(otm_obj, "otmR_func")
    if (!is.null(func_name)){
      switch (func_name,
              "BasicStats"  = ot_print_basic_stats(otm_obj, digits = digits, ...),
              "TTest"       = ot_print_ttest(otm_obj, digits = digits, ...),
              "CrossTable"  = ot_print_crosstable(otm_obj, digits = digits, ...),
              "Correlation" = ot_print_colleration(otm_obj, digits = digits, ...),
              "Glm"         = ot_print_glm(otm_obj, digits = digits, ...),
              "LogisticRegression" = ot_print_logistic_regression(otm_obj, digits = digits, ...),
              "ANOVA"       = ot_print_anova(otm_obj, digits = digits, ...),
              print(otm_obj)
      )
    }
  }
}
