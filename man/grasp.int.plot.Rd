\name{grasp.int.plot}
\alias{grasp.int.plot}
\title{Internal GRASP function }
\description{
Models resulting from grasp.step() and saved in MODELCALLS are plotted with the partial response curves of each selected explanatory variable.
}
\usage{
grasp.int.plot(gr.Yi)
}
\arguments{
  \item{gr.Yi}{A vector containing the selected responses}
}
\details{
Each response curves is the result of the backfiting algorithm used by Splus function GAM to calculate the additive contribution of each variable using non parametric smoothing methods. 
Results are expressed in the scale of the "additive predictor" before transformation into the prediction scale by the inverse link function.
Dashed lines represent upper and lower point wise twice-standard-error curves.
}
\author{ Anthony.Lehmann@unige.ch }
\seealso{ grasp \code{\link{grasp}}, grasp.in \code{\link{grasp.in}}}
\keyword{models}