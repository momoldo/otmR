\name{timevalue_missing}
\alias{timevalue_missing}
\docType{data}
\title{Sample Enquete Datasets with Missing Values}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
This data.frame shows a sample of a dataset including missing values
to be often analised: ID, questionaire items, some scores and demographic items.
}
\usage{timevalue_missing}
\format{
  A data frame with 106 observations on the following 26 variables.
  \describe{
    \item{\code{id}}{a numeric vector}
    \item{\code{Item01}-\code{Item18}}{a numeric vector}
    \item{\code{Scale01}}{a numeric vector}
    \item{\code{Scale02}}{a numeric vector}
    \item{\code{Scale03}}{a numeric vector}
    \item{\code{Scale04}}{a numeric vector}
    \item{\code{Sex}}{a character vector}
    \item{\code{Age}}{a numeric vector}
    \item{\code{Grade}}{a numeric vector}
  }
}
%\details{No details.}
%\source{The data.frame was made by Akira Harada in 2021.}
%\references{No references.}
\examples{
data(timevalue_missing)
## maybe str(timevalue_missing) ; plot(timevalue_missing) ...
}
\keyword{datasets}
