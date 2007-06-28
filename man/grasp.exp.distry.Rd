\name{grasp.exp.distry}
\alias{grasp.exp.distry}
\title{ Internal GRASP function }
\description{
This function plots the distribution of responses as histograms with additional statistics supplied
}
\usage{
grasp.exp.distry(gr.Yi,nbar = OPTIONS$NBAR)
}
\arguments{
  \item{gr.Yi}{A vector containing the selected responses}
  \item{nbar}{Number of histogram bars in the plot}
}
\details{
This function allows plotting the distribution of responses as histograms. Additional statistics are supplied. This is an important step in deciding which family of distribution to use:
-	presence-absence data 					>> binomial
-	frequency, continuous between 0 and 1 			>> binomial
-	counts, richness, positive integers 			>> poisson
-	weights, size, biomass, continuous symmetrical		>> gaussian
}
\author{ Anthony.Lehmann@unige.ch }
\seealso{ grasp \code{\link{grasp}}, grasp.in \code{\link{grasp.in}}}
\keyword{models}