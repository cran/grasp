\name{grasp.exp.cormat}
\alias{grasp.exp.cormat}
\title{ Internal GRASP function }
\description{
This function calculates the correlation between PVs and plots a correlation matrix, eliminating too highly corelated PVs.
}
\usage{
grasp.exp.cormat(gr.Yi, cols = gr.selX, thin = OPTIONS$CORTHIN, corplot = TRUE)
}
\arguments{
  \item{gr.Yi}{A vector containing the selected responses}
  \item{cols}{A vector containing the selected predictors}
  \item{thin}{Acceptable level of correlation}
  \item{corplot}{Plot it or not ?}
}
\details{
The first aim of this function is to calculate correlation between PVs and to plot a matrix of correlation. The second aim is to eliminate PVs too highly correlated. The acceptable level of correlation is decided in the OPTIONs. Limits defined by \code{grasp.msk.limits()} are applied to the data shown in these plots if desired. Selecting the reset correlation for Ys option will remove all correlation selection define in earlier session for the selected Y. This is to make sure that no correlation limits are applied by mistake.
}
\author{ Anthony.Lehmann@unige.ch }
\seealso{ grasp \code{\link{grasp}}, grasp.in \code{\link{grasp.in}}}
\keyword{models}