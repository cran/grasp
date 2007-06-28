\name{grasp.in}
\alias{grasp.in}
\title{ Generalized Regression Analysis and Spatial Prediction, initializing session}
\description{
  This function is used only when beginning with new data or when modifying names of dimensions of YYY, XXX, XXXpred
}
\usage{
grasp.in(Ymat = YYY, Xmat = XXX, Xpred = XXXpred, Xlut = NULL)
}
\arguments{
  \item{Ymat}{ is the dataframe containing the response variables (RV): Each column represents a variable and each row an observation. Columns must have unique names not included in other variable names. Data type: single, double. }
  \item{Xmat}{ is the dataframe containing the predictor variables (PV): Each column represents a variable and each rows an observation. The number of rows must be the same as for YYY. Columns must have unique names not included in other variable names. Data type: single, double, integer, factor. }
  \item{Xpred}{ is the dataframe containing the explanatory data (PV) to predict from: Each column represents a variable and each rows a new observation to predict from. Columns must have unique names identical to those in XXX and missing data must be avoided }
  \item{Xlut}{ If predictions are made on large datasets (more than 250000 pixels) it is better to use lookup tables to describe models and build predictions in Arcview. XXXlut must contain 2 rows (rows 1 and 2) with the minimum and maximum values for each variable in the prediction dataset built as Grids in Arcview. }
}
\references{ \item A. Lehmann, J.R. Leathwick & J.McC. Overton, 2002. GRASP. Ecological Modelling, 157: 189-207
\item HOMEPAGE: http://www.unige.ch/ia/climate/grasp
\item RELATED: http://www.spacedoor.net
}
\author{ Anthony.Lehmann@unige.ch }

\seealso{ grasp }
\examples{
data(YYY) # reads in YYY,XXX and XXXpred demo dataset
data(XXX)
data(XXXpred)

grasp.in(YYY,XXX,XXXpred) # initialize a new grasp session

grasp( 2:3,c(4:6,8:9), title = "GRASP: ", path = "", gr.fam = "binomial", weights = TRUE, make.summary = TRUE, plot.maps = TRUE, plot.distry = TRUE, 
plot.histograms = TRUE, plot.respvspred = TRUE, plot.xpred = TRUE,plot.correlation = TRUE, stepwise.models = TRUE, test = "AIC", contributions = TRUE, plot.contributions = TRUE, plot.models = TRUE, model.anova = TRUE, validate.models = TRUE, predictions = TRUE, plot.predictions = TRUE) # run a full grasp analysis on 2 responses with 5 predictors

}
\keyword{models}
