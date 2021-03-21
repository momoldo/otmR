#' Frequency Table
#'
#' otFrequencyTable is to make frequency tables of some variables.
#'
#' @importFrom purrr map
#' @param data a data.set object computing frequency tables
#' @param omit.na a logical indicating whether NA is to be omitted or not
#' @export

otFrequencyTable <- function(data, omit.na = TRUE){
  ot_frequency_vec <- function(x, omit.na){
    d <- data.frame(table(x, useNA = "always"))
    n_na <- d[nrow(d),2] # n of missing value

    if (omit.na||n_na==0){
      d <- d[!is.na(d[,1]),]
    }

    cum_sum <- cumsum(d[,2])
    r_freq <- d[,2]/sum(d[,2])
    r_cum_sum <- cum_sum / sum(d[,2])
    res <- data.frame(Variable=d[,1], Frequency=d[,2], Cumulative.F=cum_sum,
               Relative.F=r_freq, Cumulative.R.F=r_cum_sum)
    attr(res, "otmR_NA") <- n_na
    return(res)
  }
  if (!is.null(data)){
    res <- data %>% map(ot_frequency_vec, omit.na = omit.na)
    names(res) <- names(data)
    attr(res, "otmR_func") <- "Frequency Table"
    return(res)
  }
}
