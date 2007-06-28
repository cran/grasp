\name{grasp.exp.datamap}
\alias{grasp.exp.datamap}
\title{ Internal GRASP function }
\description{
This function is a visual check of the spatial distribution of the response data
}
\usage{
grasp.exp.datamap(gr.Yi)
}
\arguments{
  \item{gr.Yi}{A vector containing the selected responses}
}
\details{
This function has been introduced in GRASP as a visual check of the spatial distribution of the input response data. Limits defined by \code{grasp.msk.limits()} are applied to the data shown in these maps.
Geographic coordinates of points must be contained in the XXX table under column names (\code{XXX$CX}, \code{XXX$CY}).
}
\author{ Anthony.Lehmann@unige.ch }
\seealso{ grasp \code{\link{grasp}}, grasp.in \code{\link{grasp.in}}}
\keyword{models}