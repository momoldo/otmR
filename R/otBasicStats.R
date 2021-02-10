#' Computing skewness for SPSS type
#'
#' @param x a variable to summarise
#' @importFrom e1071 skewness
SKEWNESS <- function(x){
  e1071::skewness(x, na.rm = TRUE, type = 2)
}

#' Computing kurtosis for SPSS type
#'
#' @param x a variable to summarise
#' @importFrom e1071 kurtosis
KURTOSIS <- function(x){
  e1071::kurtosis(x, na.rm = TRUE, type = 2)
}

#' Computing sample size without missing values
#'
#' @param x a variable to summarise
#' @importFrom stats complete.cases
#'
Nrow <- function(x){length(x[complete.cases(x)])}

#' Computing number of missing values
#'
#' @importFrom stats complete.cases
#' @param x a variable to summarise
#'
MISSING <- function(x){length(x)-length(x[complete.cases(x)])}

#' Computing various statistics
#'
#' @importFrom stats median sd
#' @param x a variable to summarise
#'
ot_basic_stats_vec <- function(x){
  if (!is.null(x)){
    return(data.frame(
      Size = Nrow(x),
      Mean = mean(as.numeric(x), na.rm = TRUE),
      Median = median(as.numeric(x), na.rm = TRUE),
      SD = sd(as.numeric(x), na.rm = TRUE),
      Skewness = SKEWNESS(as.numeric(x)),
      Kurtosis = KURTOSIS(as.numeric(x))
    ))
  }
}

#' Computing Basic Statistics for ratio scale variables
#'
#' @param data target data.frame
#'
#' @importFrom dplyr %>%
#' @importFrom purrr map_dfr
#' @export
#'
otBasicStats <- function(data){
  res <- data %>% map_dfr(ot_basic_stats_vec)
  attr(res, "otmR_func") <- "BasicStats"
  return(res)
}
