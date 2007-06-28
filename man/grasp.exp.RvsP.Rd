\name{grasp.exp.RvsP}
\alias{grasp.exp.RvsP}
\title{ Internal GRASP function }
\description{
This function is a visual check of both response and predictor variables.
}
\usage{
grasp.exp.RvsP(gr.Yi, sX = gr.selX)
}
\arguments{
  \item{gr.Yi}{A vector of the selected responses}
  \item{sX}{A vector of the selected predictors}
}
\details{
This function is another visual check of both response (RV) and predictor variables (PV). Limits defined by grasp.msk.limits() can be applied to the data shown in these histograms if desired.
First a simple histogram of the selected explanatory variables is produced.
With binomial data, the number of response observations observed in each bar is drawn and written on top of each of them. A dashed line is added representing the overall proportion of response observations greater than 0 (presences) in comparison with the total number of observations. Finally, a continuous line is added showing whether the proportion of presences in each bar is higher or lower than the overall proportion.
}
\author{ Anthony.Lehmann@unige.ch }
\seealso{ grasp \code{\link{grasp}}, grasp.in \code{\link{grasp.in}}}
\keyword{models}