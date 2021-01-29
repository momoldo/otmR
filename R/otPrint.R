# Print out function "otBasicStatis"
#'
#' @importFrom kableExtra kbl kable_classic
#' @param otm_obj any otmR object
#'
ot_print_basic_stats <- function(otm_obj, ...){
  dg <- ifelse(is.null(list(...)[["digits"]]),getOption("digits"),
               as.integer(list(...)[["digits"]]))
  tab_caption <- ifelse(is.null(list(...)[["caption"]]),"Basic Statistics",
                        list(...)[["caption"]])
  kbl(otm_obj, digits = dg, caption = tab_caption, align = "r") %>% kable_classic(full_width=FALSE)
}

# Print out function for "otCorrelation"
#'
#' @importFrom kableExtra kbl kable_classic cell_spec
#' @param otm_obj any otmR object computed by "otCorrelation"
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

#' Print out function for otmR
#'
#' @param otm_obj An object made by ot_functions
#' @export
#'
otPrint <- function(otm_obj, ...){
  if (!is.null(otm_obj)){
    func_name <- attr(otm_obj, "otmR_func")
    if (!is.null(func_name)){
      switch (func_name,
              "BasicStats"  = ot_print_basic_stats(otm_obj, ...),
              "Correlation" = ot_print_colleration(otm_obj, ...),
              print(otm_obj)
      )
    }
  }
}
