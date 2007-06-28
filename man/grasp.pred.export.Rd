\name{grasp.pred.export}
\alias{grasp.pred.export}
\title{ Internal GRASP function }
\description{
This function will export the predictions made in grasp.pred() and stored in gr.predmat into an ASCII file ready to be read by Import Grid in ArcView or ArcGIS.
}
\usage{
grasp.pred.export(gr.Yi)
}
\arguments{
  \item{gr.Yi}{A vector containing the selected responses}
}
\details{
 The resolution of the exported GRID can be set on page R of the GUI. When observations are merging in a new cell a mean prediction is calculated (agglomeration).
}
\author{ Anthony.Lehmann@unige.ch }
\seealso{ grasp \code{\link{grasp}}, grasp.in \code{\link{grasp.in}}}
\keyword{models}