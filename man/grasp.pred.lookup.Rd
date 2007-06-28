\name{grasp.pred.lookup}
\alias{grasp.pred.lookup}
\title{ Internal GRASP function }
\description{
grasp.pred.lookup() replaces the use of both grasp.pred() and grasp.pred.export(). This function prepares lookup tables from the models built in grasp.step() and stored in MODELCALLS.
}
\usage{
grasp.pred.lookup(gr.Yi)
}
\arguments{
  \item{gr.Yi}{A vector containing the selected responses}
}
\details{
The resulting lookup table are saved under \code{OPTIONS$PATHLUT} and can be read in Arcview by an Avenue script written to build spatial predictions by reclassifying the predictors maps corresponding to those selected in the model.
}
\author{ Anthony.Lehmann@unige.ch }
\seealso{ grasp \code{\link{grasp}}, grasp.in \code{\link{grasp.in}}}
\keyword{models}
