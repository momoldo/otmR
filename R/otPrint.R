#' Print "otBasicStats" results
#'
#' @importFrom kableExtra kbl kable_classic
#' @param otm_obj an object computed by "otBasicStats"
#' @param ... further arguments passed to or from other methods.
#'
ot_print_basic_stats <- function(otm_obj, ...){
  opt <- attr(otm_obj, "otmR_options")

  kbl(otm_obj, digits = opt$digits, caption = opt$caption, align = "r") %>%
    kable_classic(full_width=FALSE)
}

#' Print "otCorrelation" results
#'
#' @importFrom kableExtra kbl kable_classic column_spec
#' @param otm_obj an object computed by "otCorrelation"
#' @param ... further arguments passed to or from other methods.
#'
ot_print_colleration <- function(otm_obj, ...){
  opt <- attr(otm_obj, "otmR_options")

  res <- otm_obj %>% kbl(digits = opt$digits, caption = opt$caption, align = "r") %>%
    kable_classic(full_width=FALSE)

  if (opt$colored){
    P <- attr(otm_obj, "otmR_P")
    for (i in 1:nrow(otm_obj)){ # res assumes [x,1+x] because otm_obj contains row.names
      res <- column_spec(res, i+1,
                         color = ifelse(otm_obj[,i]<opt$hi.correlation, "#ff99cc", "black"),
                         background = ifelse(P[,i]>opt$alpha, "white","#99ffcc"))
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
  opt <- attr(otm_obj, "otmR_options")

  if (!is.null(otm_obj)){
    tbl <- format(round(otm_obj, opt$digits), nsmall = opt$digits)
    tbl[1,6] <- sub("\\.0*","", tbl[1,6]) # adopt integer type to var.equal's t.df
    tbl[2,] <- sub("NA","", tbl[2,])      # delete character NA

    tbl <- kbl(tbl, escape = FALSE, caption = opt$caption, align = "r") %>%
        kable_classic(full_width=FALSE) %>%
        footnote(general = paste0("alpha is ",opt$alpha), general_title = "Note:")

    if (opt$colored){
      tbl <- column_spec(tbl,6:8, color = ifelse(otm_obj[,7]<opt$alpha,"black","#ff99cc"))
        # otm_obj contains row.name, so otm_obj is [2,9] but tbl assumes [2,1+9]
      tbl <- row_spec(tbl, ifelse(otm_obj[1,4]>opt$alpha,1,2), background = "#99ffcc")
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
  opt <- attr(otm_obj, "otmR_options")

  cross.tbl <- otm_obj %>% pivot_wider(names_from = 2, values_from = 3)
  row_name <- as.character(cross.tbl[,1])

  return.cross.tbl <-
    cross.tbl %>% kbl(caption = opt$caption, align = "r") %>%
    kable_classic(full_width=FALSE) %>%
    column_spec(1, border_right = TRUE, bold = TRUE)

  if (opt$colored){
    for (i in 2:ncol(cross.tbl)){
      return.cross.tbl <- column_spec(return.cross.tbl, i,
                                      color = ifelse(cross.tbl[,i]<5,"red","black"))
    }
  }

  if (!is.null(attr(otm_obj, "otmR_test"))){
    res.test <- attr(otm_obj, "otmR_test")
    tbl <- format(round(res.test[,-1], opt$digits), nsmall = opt$digits)
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
  opt <- attr(otm_obj, "otmR_options")

  fit <- attr(otm_obj, "otmR_fit")

    if (opt$colored){
    color_row <- which(otm_obj$p.value < 0.05) # 0.05 is replaced by option in the future
    tab <- kbl(otm_obj, digits = opt$digits, caption = opt$caption, align = "r") %>%
      row_spec(color_row, background = "lightgreen")

  } else {
    tab <- kbl(otm_obj, digits = opt$digits, caption = opt$caption, align = "r")
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
        kbl(digits = opt$digits, caption = "Rank of Residuals", align = "r") %>%
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
  opt <- attr(otm_obj, "otmR_options")

  fit <- attr(otm_obj, "otmR_fit")

  if (opt$colored){
    color_row <- which(otm_obj$p.value < 0.05) # 0.05 is replaced by option in the future
    tab <- kbl(otm_obj, digits = opt$digits, caption = opt$caption, align = "r") %>%
      row_spec(color_row, background = "lightgreen")

  } else {
    tab <- kbl(otm_obj, digits = opt$digits, caption = opt$caption, align = "r")
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
        kbl(digits = opt$digits, caption = "Rank of Residuals", align = "r") %>%
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
  opt <- attr(otm_obj, "otmR_options")

  if (opt$colored){
    color_row <- which(otm_obj[,5] < opt$alpha)
    tab <- kbl(otm_obj, digits = opt$digits, caption = opt$caption, align = "r") %>%
      row_spec(color_row, background = "lightgreen")
  } else {
    tab <- kbl(otm_obj, digits = opt$digits, caption = opt$caption, align = "r")
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

    dg <- ifelse(is.null(digits),getOption("digits"), digits)

    c.model <- attr(otm_obj, "otmR_model")
    tab_caption <- ifelse(is.null(list(...)[["caption"]]),
                          paste0("Result of ", func_name), list(...)[["caption"]])
    if (!is.null(c.model)){
      c.model <- as.character(c.model)
      tab_caption <- paste0(tab_caption, "(",c.model[2],"~",c.model[3],")")
    }
    is.colored <- # coloring option
      ifelse(is.null(list(...)[["is.colored"]]),
             TRUE, as.logical(list(...)[["is.colored"]]))
    alpha <- # significant level, otTTest otCorrelation
      ifelse(is.null(attr(otm_obj, "otmR_alpha")),
             0.05, as.numeric(attr(otm_obj, "otmR_alpha")))
    hi.correlation <- # otCorrelation
      ifelse(is.null(list(...)[["hi.correlation"]]),
             0.35, as.numeric(list(...)[["hi.correlation"]]))

    attr(otm_obj, "otmR_options") <- list(
      digits   = dg,
      caption  = tab_caption,
      colored  = is.colored,
      alpha    = alpha,
      hi.correlation = hi.correlation
    )

    if (!is.null(func_name)){
      switch (func_name,
              "BasicStats"  = ot_print_basic_stats(otm_obj, ...),
              "TTest"       = ot_print_ttest(otm_obj, ...),
              "CrossTable"  = ot_print_crosstable(otm_obj, ...),
              "Correlation" = ot_print_colleration(otm_obj, ...),
              "Glm"         = ot_print_glm(otm_obj, ...),
              "LogisticRegression" = ot_print_logistic_regression(otm_obj, ...),
              "ANOVA"       = ot_print_anova(otm_obj, ...),
              print(otm_obj)
      )
    }
  }
}
