\name{grasp.step.cross}
\alias{grasp.step.cross}
\title{ Internal GRASP function }
\description{
  Stepwise CROSS selection: A cross validation method is used at each step of a BIC selection to keep at the end the model presenting the highest statistics of cross validation.
}
\usage{
grasp.step.cross(object, scope, scale, direction = c("both", "backward", "forward"), trace = TRUE, keep = NULL, steps = 50, ...)
}
\arguments{
  \item{object}{...}
  \item{scope}{...}
  \item{scale}{...}
  \item{direction}{...}
  \item{trace}{If true, verbose output}
  \item{keep}{...}
  \item{steps}{Number of stepwise steps}
  \item{...}{...}
}
\author{ Anthony.Lehmann@unige.ch }
\seealso{ grasp \code{\link{grasp}}, grasp.in \code{\link{grasp.in}}}
\keyword{models}