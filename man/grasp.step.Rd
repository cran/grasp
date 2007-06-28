\name{grasp.step}
\alias{grasp.step}
\title{ Internal GRASP function }
\description{
The function selects significant predictors for each response variable
}
\usage{
grasp.step(gr.Yi)
}
\arguments{
  \item{gr.Yi}{A vector containing the selected responses}
}
\details{
  This function is the central part of GRASP. It selects significant predictors for each response variable. By default the stepwise procedure starts with a full model containing all potential variables (selX), and goes in loop to try to eliminate one variable at the time. At each step, the less significant variable is dropped from the model, and the loop starts again with the remaining variables. Tests are used to decide whether a variable should be dropped, kept or reintroduced. Available tests are : AIC, BIC, F, Chisq, BRUTO and CROSS

}
\author{ Anthony.Lehmann@unige.ch }
\seealso{ grasp \code{\link{grasp}}, grasp.in \code{\link{grasp.in}}}
\keyword{models}