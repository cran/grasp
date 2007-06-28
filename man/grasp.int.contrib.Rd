\name{grasp.int.contrib}
\alias{grasp.int.contrib}
\title{ Internal GRASP function }
\description{
Calculates the contributions...
}
\usage{
grasp.int.contrib(gr.Yi)
}
\arguments{
  \item{gr.Yi}{A vector containing the selected responses}
}
\details{
  Three methods are used to calculate contributions:
-Drop contributions are obtained by dropping each explanatory variable from the model and by calculating the associated change in deviance. Drop contributions indicate the marginal contribution of each variable.
-Alone contributions are calculated by creating new models with only one predictor. Model contributions are calculated by the range between maximum and minimum contribution of each variable in the linear predictor scale before transformation by the link function. Alone contributions better reflect the potential of each variable. 
-Model contribution is the only measure which gives an indication of the contribution of the variable within the selected model. It corresponds to the possible range of variation on the scale of the linear predictor as shown on the vertical axes of plots produced by grasp.int.plot (see in next paragraphs)
}
\author{ Anthony.Lehmann@unige.ch }
\seealso{ grasp \code{\link{grasp}}, grasp.in \code{\link{grasp.in}}}
\keyword{models}