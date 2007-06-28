\name{grasp.mod.validate}
\alias{grasp.mod.validate}
\title{ Internal GRASP function }
\description{
Function allows a visual check of the relationship between fitted and observed data.
}
\usage{
grasp.mod.validate(gr.Yi)
}
\arguments{
  \item{gr.Yi}{A vector containing the selected responses}
}
\details{
A cross-validation is made with subsets of the entire dataset, where each subset contains an equal number of randomly selected data points. Each subset is then dropped from the model, the model is recalculated and predictions made for the omitted data points. Combination of the predictions from the different subsets is then plotted against the observed data. A ROC (area under curve) and a COR (Spearman Correlation) statistics is used with binomial data.(see Fielding & Bell, 1997). A simple correlation coefficient is calculated for Poisson and normally distributed data.
}
\author{ Anthony.Lehmann@unige.ch }
\seealso{ grasp \code{\link{grasp}}, grasp.in \code{\link{grasp.in}}}
\keyword{models}