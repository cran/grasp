\name{grasp.step.bruto}
\alias{grasp.step.bruto}
\title{ Internal GRASP function }
\description{
 Stepwise BRUTO selection
}
\usage{
grasp.step.bruto(gr.Yi, selectedX)
}
\arguments{
  \item{gr.Yi}{A vector containing the selected responses}
  \item{selectedX}{Selected predictors}
}
\details{
Use the bruto methods found in library mda at: \url{http://www.stats.ox.ac.uk/pub/MASS4/Software.html}.
}
\author{ Anthony.Lehmann@unige.ch }
\seealso{ grasp \code{\link{grasp}}, grasp.in \code{\link{grasp.in}}}
\keyword{models}