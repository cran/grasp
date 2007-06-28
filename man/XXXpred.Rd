\name{XXXpred}
\alias{XXXpred}
\docType{data}
\title{ XXXpred: contains predictor variables used in GRASP to predict from}
\description{
  XXXpred is the dataframe containing the explanatory data (PV) to predict from: Each column represents a variable and each rows a new observation to predict from.
}
\usage{data(XXXpred)}
\format{
  A data frame with 6620 observations on the following 12 variables.
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
 Columns must have unique names identical to those in XXX and missing data must be avoided.
Categorical variables must be transformed to factor variable in XXX and XXXpred.
}
\source{
  http://www.cscf.ch
}
\examples{
data(XXXpred)
summary(XXXpred)
}
\keyword{datasets}
