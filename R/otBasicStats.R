# Calculate skewness for SPSS type
#' @importFrom e1071 skewness
Skewness <- function(x){
  e1071::skewness(x, na.rm = TRUE, type = 2)
}

# Calculate kurtosis for SPSS type
#' @importFrom e1071 kurtosis
Kurtosis <- function(x){
  e1071::kurtosis(x, na.rm = TRUE, type = 2)
}

#' Calculating Basic Statistics for ratio scale variables
#'
#' @param data target data.frame
#'
#' @importFrom magrittr %>%
#' @importFrom e1071 skewness
#' @importFrom e1071 kurtosis
#' @import modelsummary
#' @importFrom kableExtra kable_classic
#' @export
#'
otBasicStats <- function(data){
  data %>% datasummary(All(.)~N+Min+Max+Mean+Median+SD+Skewness+Kurtosis+Histogram, data = .,
                       output = "kableExtra", title = "Basic Statistics") %>%
    kable_classic(full_width = FALSE)
}
