#' Frequency Table
#'
#' otFrequencyTable is to make frequency tables of some variables.
#'
#' @importFrom dplyr %>%
#' @importFrom purrr map
#' @param data a data.set object computing frequency tables
#' @param omit.na a logical indicating whether NA is to be omitted or not
#' @export

otFrequencyTable <- function(data, omit.na = FALSE){
  ot_frequency_vec <- function(x, useNA){
    d <- data.frame(table(x, useNA = useNA))
    cum_sum <- cumsum(d[,2])
    r_freq <- d[,2]/sum(d[,2])
    r_cum_sum <- cum_sum / sum(d[,2])
    data.frame(Variable=d[,1], Frequency=d[,2], Cumulative.F=cum_sum,
               Relative.F=r_freq, Cumulative.R.F=r_cum_sum)
  }
  if (!is.null(data)){
    na_action <- ifelse(omit.na, "no", "ifany")
    res <- data %>% map(ot_frequency_vec, useNA = na_action)
    names(res) <- names(data)
    return(res)
  }
}
