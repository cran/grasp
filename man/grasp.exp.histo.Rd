\name{grasp.exp.histo}
\alias{grasp.exp.histo}
\title{ Internal GRASP function }
\description{
This function plots the responses vs. the predictors.
}
\usage{
grasp.exp.histo(gr.Yi, sX = gr.selX, nbar = OPTIONS$NBAR)
}
\arguments{
  \item{gr.Yi}{A vector containing the selected responses}
  \item{sX}{A vector of the selected predictors}
  \item{nbar}{Number of histogram bars in the pot}
}
\details{
This function is plots responses (RV) versus predictor variables (PV). Limits defined by \code{grasp.msk.limits()} are applied to the data shown in these histograms.
A smoothing function is applied (spline) to the relationship between the responses and each continuous predictor. The degrees of freedom for the smooth function is the same as the one required to build the model and saved in \code{OPTIONS$DF2}.
Weights are applied when WEIGHTS matrix contains values different from 1
}
\author{ Anthony.Lehmann@unige.ch }
\seealso{ grasp \code{\link{grasp}}, grasp.in \code{\link{grasp.in}}}
\keyword{models}