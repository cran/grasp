\name{grasp.step.anova}
\alias{grasp.step.anova}
\title{ Internal GRASP function }
\description{
Stepwise F and Chisq test selection
}
\usage{
grasp.step.anova(object, scope, scale, direction = c("both", "backward", "forward"),
                 trace = TRUE, keep = NULL, steps = 1000, ...)
}
\arguments{
  \item{object}{...}
  \item{scope}{...}
  \item{scale}{...}
  \item{direction}{...}
  \item{trace}{If true, verbose output}
  \item{keep}{...}
  \item{steps}{Number of stepwise steps}
  \item{...}{...}
}
\details{
Stepwise F and Chisq test selection: 
- F: an F test is used to test whether predictors are significant (for quasi models) 
- CHI: an Chi test is used to test whether predictors are significant variable
}
\author{ Anthony.Lehmann@unige.ch }
\seealso{ grasp \code{\link{grasp}}, grasp.in \code{\link{grasp.in}}}
\keyword{models}