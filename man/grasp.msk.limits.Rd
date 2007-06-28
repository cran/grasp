\name{grasp.msk.limits}
\alias{grasp.msk.limits}
\title{ Internal GRASP function }
\description{
It restricts the data within limits defined for selected response variables by keeping a given number of 0 observations on each side of the last presence along selected predictors.
}
\usage{
grasp.msk.limits(gr.Yi, sX = OPTIONS$SELXLIM, npast = OPTIONS$NPAST)
}
\arguments{
  \item{gr.Yi}{A vector containing the selected responses}
  \item{sX}{Defined in the options}
  \item{npast}{Defined in the options}
}
\details{
  The \code{grasp.msk.limits()} function is optional. It restricts the data within limits defined for selected response variables by keeping a given number of 0 observations on each side of the last presence along selected predictors (Austin and Nichols, 1997; Leathwick, 1998). You can change the X variables used to set the limits and the number of observations kept on each side of the last positive observations. 
}
\author{ Anthony.Lehmann@unige.ch }
\seealso{ grasp \code{\link{grasp}}, grasp.in \code{\link{grasp.in}}}
\keyword{models}