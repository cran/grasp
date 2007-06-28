\name{grasp.exp.xpredplot}
\alias{grasp.exp.xpredplot}
\title{ Internal GRASP function }
\description{
This function maps the predictors stored in XXXpred at the resolution set in the GUI
}
\usage{
grasp.exp.xpredplot(gr.Xi)
}
\arguments{
  \item{gr.Xi}{A vector containing the selected predictors}
}
\details{
Aggregation of pixels is calculated if the chosen resolution is coarser than the actual resolution in XXXpred.
}
\author{ Anthony.Lehmann@unige.ch }
\seealso{ grasp \code{\link{grasp}}, grasp.in \code{\link{grasp.in}}}
\keyword{models}