\name{grasp.pred}
\alias{grasp.pred}
\title{ Internal GRASP function }
\description{
 It predicts each Response Variables (RVs) using a new set of Predictor Variables (PVs) contained in XXXpred.
}
\usage{
grasp.pred(gr.Yi)
}
\arguments{
  \item{gr.Yi}{A vector containing the selected responses}
}
\details{
 This function is certainly the most demanding in time and computer resources. Its purpose is to predict each Response Variables (RVs) using a new set of Predictor Variables (PVs) contained in XXXpred. It uses the models selected in \code{grasp.step()} and stored in MODELCALLS. Predictions are calculated using the Splus function \code{pred.gam(MODELCALLS,XXXpred,type="response")}. When type="response" is selected, the predictions are on the scale of the response, and are monotone transformations of the additive predictors, using the inverse link function. 
}
\author{ Anthony.Lehmann@unige.ch }
\seealso{ grasp \code{\link{grasp}}, grasp.in \code{\link{grasp.in}}}
\keyword{models}