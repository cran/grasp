\name{grasp.step.contrib}
\alias{grasp.step.contrib}
\title{ Internal GRASP function }
\description{
 Stepwise CONTRIB selection: Stepwise selection absed on minimum percentage of Model contribution.
}
\usage{
grasp.step.contrib(object, scope, scale, direction = c("both", "backward", "forward"), trace = TRUE, keep = NULL, steps = 1000, ...)
}
\arguments{
  \item{object}{...}
  \item{scope}{...}
  \item{scale}{...}
  \item{direction}{...}
  \item{trace}{If true, verbose output}
  \item{keep}{...}
  \item{steps}{Number of stepwise steps}
  \item{...}{Additional arguments to be passsed on to 'keep'}
}
\author{ Anthony.Lehmann@unige.ch }
\seealso{ grasp \code{\link{grasp}}, grasp.in \code{\link{grasp.in}}}
\keyword{models}