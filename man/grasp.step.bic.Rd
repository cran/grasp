\name{grasp.step.bic}
\alias{grasp.step.bic}
\title{ Internal GRASP function }
\description{
 Stepwise BIC selection: a Bayesian IC criteria is used to select variables, very selective
}
\usage{
grasp.step.bic(object, scope, scale, direction = c("both", "backward", "forward"), trace = TRUE, keep = NULL, steps = 1000, ...)
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
\author{ Anthony.Lehmann@unige.ch }
\seealso{ grasp \code{\link{grasp}}, grasp.in \code{\link{grasp.in}}}
\keyword{models}
