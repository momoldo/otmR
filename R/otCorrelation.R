#' Computing pairwise N
#'
#' @importFrom stats complete.cases
#' @param data a data.frame
#'
ot_pairwise_n <- function(data){
  n_variables <- ncol(data)
  s_pair <- matrix(0, nrow = n_variables, ncol = n_variables)
  for (i in 1:n_variables){
    for (j in 1:n_variables){
      s_pair[i,j] <- nrow(data[complete.cases(data[,c(i,j)]),])
    }
  }
  return(s_pair)
}

#' Computing correlation matrix from data.frame
#'
#' @param data a data.frame that contains numeric variables only
#' @param use an optional character string which is one of the strings
#'   "everything", "all.obs", "complete.obs", na.or.complete", or
#'   "pairwise.complete.obs".  See the heflp "cor".
#' @param method a character string indicating which correlation coefficient is
#'   to be computed.  One of "pearson", "kendall", or "spearman" can be abbreviated.
#' @importFrom dplyr %>%
#' @importFrom stats cor pt
#' @examples otCorrelation(timevalue[,c(2:11)]) %>% otPrint(digits=3) # show correlation matrix
#' @export
#'
otCorrelation <- function(data, use = "pairwise.complete.obs",
                          method = c("pearson","kendall","spearman")){
  res <- data %>% cor(use = use, method = method)
  N <- ot_pairwise_n(data)
  DF <- N-2
  t.value <- res/sqrt(1-res^2)*sqrt(DF)
  p.value <- (1-pt(abs(t.value),DF))*2
  attr(res,"otmR_N") <- N
  attr(res, "otmR_DF") <- DF
  attr(res, "otmR_T") <- round(t.value,3)
  attr(res, "otmR_P") <- round(p.value,3)
  attr(res,"otmR_func") <- "Correlation"
  return(res)
}
