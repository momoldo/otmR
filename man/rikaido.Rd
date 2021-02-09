\name{rikaido}
\alias{rikaido}
\docType{data}
\title{A sample data.frame to check otANOVA correctly}
\description{
A data.frame "rikaido" consists of 4 variables: three nominal scales and
one ratio scale.  They are used to test two-way ANOVA model.
}
\usage{rikaido}
\format{
  A data frame with 24 observations on the following 4 variables.
  \describe{
    \item{\code{id}}{a numeric vector}
    \item{\code{attitude}}{a numeric vector}
    \item{\code{grace}}{a numeric vector}
    \item{\code{score}}{a numeric vector}
  }
}
\examples{
data(rikaido)
## maybe str(rikaido) ; plot(rikaido) ...
}
\keyword{datasets}
