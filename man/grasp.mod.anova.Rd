\name{grasp.mod.anova}
\alias{grasp.mod.anova}
\title{ Internal GRASP function }
\description{
An ANOVA table is constructed by testing the significance of removing in turn each predictor from the selected model.
}
\usage{
grasp.mod.anova(gr.Yi)
}
\arguments{
  \item{gr.Yi}{A vector containing the selected responses}
}
\details{
This function gives a different result from the anova function of Splus \code{anova(model, test="F")}, which tests the successive inclusion of variables into the model and is therefore dependant on the order of variable entry.
}
\author{ Anthony.Lehmann@unige.ch }
\seealso{ grasp \code{\link{grasp}}, grasp.in \code{\link{grasp.in}}}
\keyword{models}