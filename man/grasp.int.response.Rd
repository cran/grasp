\name{grasp.int.response}
\alias{grasp.int.response}
\title{ Internal GRASP function }
\description{
This complex function allows simultaneous plotting of predictions from different responses in relation to continuous predictor variables
}
\usage{
grasp.int.response(sY,sX)
}
\arguments{
  \item{sY}{A vector containing the selected responses}
  \item{sX}{A vector of the selected predictors}
}
\details{
This complex function allows simultaneous plotting of predictions from different responses in relation to continuous predictor variables. The result is in the scale of the responses. Better results are obtained if the \code{OPTIONS$RECALCULATEWEIGHTS} (on page A) is set to TRUE, as this gives the same weights to presences and absences in the analyses.
The contributions of all selected predictors in a model are set to 0 except for one predictor. A new prediction is made for that predictor given a 0 contribution of all other variables
}
\author{ Anthony.Lehmann@unige.ch }
\seealso{ grasp \code{\link{grasp}}, grasp.in \code{\link{grasp.in}}}
\keyword{models}