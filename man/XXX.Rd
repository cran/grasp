\name{XXX}
\alias{XXX}
\docType{data}
\title{ XXX: contains predictor variables for GRASP}
\description{
  XXX is the dataframe containing the predictor variables (PV): Each column represents a variable and each rows an observation.
}
\usage{data(XXX)}
\format{
  A data frame with 7563 observations on the following 12 variables.
  \describe{
    \item{ID}{a numeric vector, Unique ID, in same order as in YYY}
    \item{CX}{a numeric vector, X geographic coordinate}
    \item{CY}{a numeric vector, Y geographic coordinate}
    \item{TEMP}{a numeric vector, Mean annual temperature}
    \item{RAD}{a numeric vector, Mean annual solar radiation}
    \item{PREC}{a numeric vector, Precipitation}
    \item{NNESS}{a numeric vector, Northness}
    \item{SLOPE}{a numeric vector, Slope}
    \item{ASPECT}{a factor with levels \code{NORTH} \code{NULL} \code{SOUTH}}
  }
}
\details{
 The number of rows must be the same as for YYY. Columns must have unique names not included in other variable names. Data type: single, double, integer, factor.
}
\source{
    http://www.cscf.ch
}
\seealso{YYY, XXXpred}
\examples{
data(XXX)
summary(XXX)
}
\keyword{datasets}
