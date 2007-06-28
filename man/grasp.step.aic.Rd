\name{grasp.step.aic}
\alias{grasp.step.aic}
\title{Internal GRASP function}
\description{
Stepwise AIC selection: an Akaike IC criteria is used to select variables, very conservative (default test)
}
\synopsis{
grasp.step.aic(object, scope, scale, direction = c("both","backward","forward"),
trace = TRUE, keep = NULL, steps = 1000, ...)
}
\usage{
grasp.step.aic(object, scope, scale, direction, trace, keep, steps, ...)
}
\arguments{
  \item{object}{An object of class 'gam' or any of it's inheritants.}
  \item{scope}{...}
  \item{scale}{...}
  \item{direction}{...}
  \item{trace}{If true, verbose output}
  \item{keep}{...}
  \item{steps}{Number of stepwise steps}
  \item{...}{...}
}
\author{
Function step.gam by Trevor Hastie (\code{gam} library).
Modified by Anthony lehmann \email{Anthony.Lehmann@unige.ch}.
}
\seealso{
  \code{\link{step.gam}} for the generic function.
}
\keyword{models}
