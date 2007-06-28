\name{YYY}
\alias{YYY}
\docType{data}
\title{ YYY: contains response variables for GRASP}
\description{
  YYY is the trial dataframe containing the response variables (RV): Each column represents a variable and each row an observation.
}
\usage{data(YYY)}
\format{
  A data frame with 7563 observations on the following 4 variables.
  \describe{
    \item{ID}{a numeric vector, Unique ID, in same order as in XXX}
    \item{SP1}{a numeric vector, example of binomial data with presence/absence of species 1}
    \item{SP2}{a numeric vector, example of binomial data with presence/absence of species 2}
    \item{RICHNESS}{a numeric vector, example of Poisson data with species richness}
  }
}
\details{
 Columns must have unique name not included in other variable names. Data type: single, double. Data distribution: Binomial, Poisson, Gaussian
}
\source{
  http://www.cscf.ch
}
\examples{
data(YYY)
summary(YYY)
}
\keyword{datasets}